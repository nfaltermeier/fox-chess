use std::fs;
use std::io::Write;
use std::{cmp::Reverse, fs::File, i16, path::PathBuf, sync::mpsc::Receiver, time::Instant};

use log::{debug, error};
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_MASK},
    evaluate::{CENTIPAWN_VALUES, ENDGAME_GAME_STAGE_FOR_QUIESCENSE, MATE_THRESHOLD, MATE_VALUE},
    move_generator::{ScoredMove, MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_KILLER_1, MOVE_SCORE_KILLER_2},
    moves::{Move, MoveRollback, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_CAPTURE_FULL},
    repetition_tracker::RepetitionTracker,
    transposition_table::{self, MoveType, TTEntry, TableType, TranspositionTable},
    uci::UciInterface,
};

pub type HistoryTable = [[[i16; 64]; 6]; 2];
pub static DEFAULT_HISTORY_TABLE: HistoryTable = [[[0; 64]; 6]; 2];

const EMPTY_MOVE: Move = Move { data: 0 };
const DEBUG_BOARD_HASH_OF_INTEREST: Option<u64> = None;

// pub const TEST_TT_FOR_HASH_COLLISION: bool = true;

#[derive(Default)]
pub struct SearchStats {
    pub quiescense_nodes: u64,
    pub depth: u8,
    pub quiescense_cut_by_hopeless: u64,
    pub leaf_nodes: u64,
}

#[derive(PartialEq, Eq)]
enum SearchControl {
    Time,
    Depth,
    Infinite,
}

pub struct SearchResult {
    pub best_move: Move,
    pub eval: i16,
    pub stats: SearchStats,
    pub pv: Vec<Move>,
}

pub struct AlphaBetaResult {
    pub search_result: Option<SearchResult>,
    pub end_search: bool,
    pub stop_received: bool,
}

impl Board {
    pub fn iterative_deepening_search(
        &mut self,
        time: &Option<UciTimeControl>,
        search: &Option<UciSearchControl>,
        transposition_table: &mut TranspositionTable,
        history_table: &mut HistoryTable,
        stop_rx: &Receiver<()>,
    ) -> SearchResult {
        let start_time = Instant::now();
        let mut target_dur = None;
        let search_control: SearchControl;
        let mut max_depth = 40;

        debug!("starting search of {}", self.to_fen());

        if let Some(t) = time {
            match t {
                UciTimeControl::TimeLeft {
                    white_time,
                    black_time,
                    white_increment,
                    black_increment,
                    moves_to_go,
                } => {
                    let time_left = if self.white_to_move { white_time } else { black_time };

                    if time_left.is_none() {
                        error!("No time left value provided when searching");
                        panic!("No time left value provided when searching");
                    }

                    let divisor = if self.fullmove_counter < 10 {
                        21
                    } else if self.fullmove_counter < 20 {
                        18
                    } else {
                        25
                    };
                    target_dur = Some(
                        time_left
                            .as_ref()
                            .unwrap()
                            .to_std()
                            .unwrap()
                            .checked_div(divisor)
                            .unwrap(),
                    );

                    // maybe something like https://www.desmos.com/calculator/47t9iys2fo
                    // let expected_moves_left = if let Some(mtg) = moves_to_go {
                    //     *mtg
                    // } else {
                    //     let eval = self.evaluate();
                    // }

                    search_control = SearchControl::Time;
                }
                UciTimeControl::MoveTime(time_delta) => {
                    target_dur = Some(time_delta.to_std().unwrap());
                    search_control = SearchControl::Time;
                }
                UciTimeControl::Ponder => {
                    unimplemented!("uci go ponder");
                }
                UciTimeControl::Infinite => {
                    search_control = SearchControl::Infinite;
                }
            }
        } else if let Some(s) = search {
            if s.depth.is_some() {
                search_control = SearchControl::Depth;
                max_depth = s.depth.unwrap();

                if fs::exists("eval_trees").unwrap() == false {
                    fs::create_dir("eval_trees").unwrap();
                }
            } else {
                error!("Unsupported search option passed to go, use depth.");
                unimplemented!("Unsupported search option passed to go, use depth.");
            }
        } else {
            error!("One of search or time is required to be passed to go.");
            panic!("One of search or time is required to be passed to go.");
        }

        // Above 5 depth moves can start taking a lot more time
        let cutoff_low_depth;
        let cutoff;
        let cancel_search_time;
        match target_dur {
            Some(d) => {
                cutoff_low_depth = Some(d.mul_f32(0.55));
                cutoff = Some(d.mul_f32(0.35));
                cancel_search_time = Some(start_time.checked_add(d.mul_f32(2.0)).unwrap());
            }
            None => {
                cutoff = None;
                cutoff_low_depth = None;
                cancel_search_time = None;
            }
        }
        let mut depth = 1;
        let mut latest_result = None;
        loop {
            let mut eval_tree_file = if search_control == SearchControl::Depth {
                Some(
                    File::create(PathBuf::from(format!(
                        "eval_trees/{:03}_[{}]_depth_{:02}.txt",
                        (self.fullmove_counter - 1) * 2 + if self.white_to_move { 0 } else { 1 },
                        self.to_fen().replace("/", "."),
                        depth
                    )))
                    .unwrap(),
                )
            } else {
                None
            };

            let result = self.alpha_beta_init(
                depth,
                transposition_table,
                history_table,
                &cancel_search_time,
                stop_rx,
                search_control == SearchControl::Infinite,
                &mut eval_tree_file,
            );
            if let Some(search_result) = result.search_result {
                let elapsed = start_time.elapsed();

                // print less when using TT values
                if search_result.stats.leaf_nodes != 0 || depth == 1 {
                    UciInterface::print_search_info(
                        search_result.eval,
                        &search_result.stats,
                        &elapsed,
                        &search_result.pv,
                    );
                }

                if search_control != SearchControl::Infinite
                    && (result.end_search
                        || search_result.eval.abs() >= MATE_THRESHOLD
                        || depth >= max_depth
                        || match search_control {
                            SearchControl::Time => {
                                (depth >= 5 && elapsed >= cutoff.unwrap()) || elapsed >= cutoff_low_depth.unwrap()
                            }
                            SearchControl::Depth | SearchControl::Infinite => false,
                        })
                {
                    return search_result;
                }

                // This seems like it could go really wrong. For when entire search is skipped by tt best move node.
                // if search_result.stats.depth > depth {
                //     depth = search_result.stats.depth;
                // }

                latest_result = Some(search_result);
            } else if !result.stop_received {
                debug!("Cancelled search of depth {depth} due to exceeding time budget");
                return latest_result
                    .expect("iterative_deepening_search exceeded cancel_search_time before completing any searches");
            } else {
                return latest_result.expect("stop received before completing any searches");
            }

            depth += 1;
        }
    }

    pub fn alpha_beta_init(
        &mut self,
        draft: u8,
        transposition_table: &mut TranspositionTable,
        history_table: &mut HistoryTable,
        cancel_search_at: &Option<Instant>,
        stop_rx: &Receiver<()>,
        infinite_search: bool,
        eval_tree_file: &mut Option<File>,
    ) -> AlphaBetaResult {
        let mut alpha = -i16::MAX;
        let mut best_value = -i16::MAX;
        let mut best_move = None;
        let mut rollback = MoveRollback::default();
        let mut stats = SearchStats::default();
        let mut killers = [EMPTY_MOVE, EMPTY_MOVE];
        let mut pv = Vec::new();
        let mut line = Vec::new();
        let mut move_tree = Vec::new();

        let tt_entry = transposition_table.get_entry(self.hash, TableType::Main);
        if let Some(tt_data) = tt_entry {
            // if tt_data.draft >= draft {
            //     if let transposition_table::MoveType::Best = tt_data.move_type {
            //         // should this be done for the root????
            //         stats.depth = tt_data.draft;
            //         return AlphaBetaResult {
            //             search_result: Some(SearchResult {
            //                 best_move: tt_data.important_move,
            //                 eval: tt_data.eval * if self.white_to_move { 1 } else { -1 },
            //                 stats,
            //             }),
            //             end_search: false,
            //         };
            //     }
            // }

            self.make_move(&tt_data.important_move, &mut rollback);
            move_tree.push(tt_data.important_move.simple_long_algebraic_notation());

            let result;
            if self.halfmove_clock >= 100
                || RepetitionTracker::test_threefold_repetition(self)
                || self.is_insufficient_material()
            {
                result = 0;
            } else {
                result = -self.alpha_beta_recurse(
                    -i16::MAX,
                    -alpha,
                    draft - 1,
                    1,
                    &mut rollback,
                    &mut stats,
                    transposition_table,
                    &mut killers,
                    history_table,
                    &mut line,
                    eval_tree_file,
                    &mut move_tree,
                );
            }

            self.unmake_move(&tt_data.important_move, &mut rollback);
            move_tree.pop();

            if result > best_value {
                best_value = result;
                best_move = Some(tt_data.important_move);
                if result > alpha {
                    alpha = result;
                }

                line.push(tt_data.important_move);
                pv = line.clone();
            }
        }

        let mut legal_moves: u16 = 0;
        let mut moves = self.generate_pseudo_legal_moves_with_history(history_table);

        moves.sort_unstable_by_key(|m| Reverse(m.score));

        for r#move in moves {
            if tt_entry.is_some_and(|v| v.important_move == r#move.m) {
                // Hash move is assumed to be legal
                legal_moves += 1;
                continue;
            }

            let stop_received = matches!(stop_rx.try_recv(), Ok(()));
            if stop_received || cancel_search_at.is_some_and(|t| Instant::now() >= t) {
                return AlphaBetaResult {
                    search_result: None,
                    end_search: true,
                    stop_received: true,
                };
            }

            let (legal, move_made) = self.test_legality_and_maybe_make_move(r#move.m, &mut rollback);
            if !legal {
                if move_made {
                    self.unmake_move(&r#move.m, &mut rollback);
                }

                continue;
            } else {
                legal_moves += 1;
            }

            move_tree.push(r#move.m.simple_long_algebraic_notation());

            let result;
            if self.halfmove_clock >= 100
                || RepetitionTracker::test_threefold_repetition(self)
                || self.is_insufficient_material()
            {
                result = 0;
            } else {
                result = -self.alpha_beta_recurse(
                    -i16::MAX,
                    -alpha,
                    draft - 1,
                    1,
                    &mut rollback,
                    &mut stats,
                    transposition_table,
                    &mut killers,
                    history_table,
                    &mut line,
                    eval_tree_file,
                    &mut move_tree,
                );
            }

            self.unmake_move(&r#move.m, &mut rollback);
            move_tree.pop();

            if result > best_value {
                best_value = result;
                best_move = Some(r#move.m);
                if result > alpha {
                    alpha = result;
                }

                line.push(r#move.m);
                pv = line.clone();
            }
        }

        if legal_moves == 0 {
            error!(
                "Tried to search on a position but found no moves. Position: {:#?}",
                self
            );
            panic!("Tried to search on a position but found no moves");
        }

        if let Some(e) = eval_tree_file {
            writeln!(e, "eval_inherit {} hash {:#018x}", best_value, self.hash).unwrap();
        }

        if legal_moves == 1 && !infinite_search {
            stats.depth = 1;

            let bm = best_move.unwrap();

            return AlphaBetaResult {
                search_result: Some(SearchResult {
                    best_move: bm,
                    eval: self.evaluate(),
                    stats,
                    pv: [bm].to_vec(),
                }),
                end_search: true,
                stop_received: false,
            };
        } else {
            stats.depth = draft;
        }

        // Make the score not side-to-move relative
        AlphaBetaResult {
            search_result: Some(SearchResult {
                best_move: best_move.unwrap(),
                eval: best_value * if self.white_to_move { 1 } else { -1 },
                stats,
                pv,
            }),
            end_search: false,
            stop_received: false,
        }
    }

    fn alpha_beta_recurse(
        &mut self,
        mut alpha: i16,
        beta: i16,
        draft: u8,
        ply: u8,
        rollback: &mut MoveRollback,
        stats: &mut SearchStats,
        transposition_table: &mut TranspositionTable,
        killers: &mut [Move; 2],
        history_table: &mut HistoryTable,
        pv: &mut Vec<Move>,
        eval_tree_file: &mut Option<File>,
        move_tree: &mut Vec<String>,
    ) -> i16 {
        if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
            debug!("Board hash of interest found: {self:#?}")
        }

        if draft == 0 {
            stats.leaf_nodes += 1;

            pv.clear();

            let score =
                self.quiescense_side_to_move_relative(alpha, beta, ply + 1, rollback, stats, transposition_table);

            if let Some(e) = eval_tree_file {
                writeln!(e, "r/{}/eval {} hash {:#018x}", move_tree.join("/"), score, self.hash).unwrap();
            }

            return score;
        }

        let mut best_value = -i16::MAX;
        let mut best_move = None;
        let mut new_killers = [EMPTY_MOVE, EMPTY_MOVE];
        let mut line = Vec::new();

        let tt_entry = transposition_table.get_entry(self.hash, TableType::Main);
        if let Some(tt_data) = tt_entry {
            if tt_data.draft >= draft {
                let mut eval = tt_data.eval;

                if eval >= MATE_THRESHOLD {
                    eval -= 10 * ply as i16;
                } else if eval <= -MATE_THRESHOLD {
                    eval += 10 * ply as i16;
                }

                match tt_data.move_type {
                    transposition_table::MoveType::FailHigh => {
                        if eval >= beta {
                            self.update_killers_and_history(killers, &tt_data.important_move, history_table, ply);

                            if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                                debug!(
                                    "Board hash of interest using evaluation from tt fail high: {} {:04x}",
                                    tt_data.important_move.pretty_print(Some(self)),
                                    tt_data.important_move.data
                                );
                            }

                            if let Some(e) = eval_tree_file {
                                writeln!(
                                    e,
                                    "r/{}/eval_high_tt {} hash {:#018x}",
                                    move_tree.join("/"),
                                    eval,
                                    self.hash
                                )
                                .unwrap();
                            }

                            return eval;
                        }
                    }
                    transposition_table::MoveType::Best => {
                        if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                            debug!(
                                "Board hash of interest using evaluation from tt best move: {} {:04x}",
                                tt_data.important_move.pretty_print(Some(self)),
                                tt_data.important_move.data
                            );
                        }

                        if let Some(e) = eval_tree_file {
                            writeln!(e, "r/{}/eval_tt {} hash {:#018x}", move_tree.join("/"), eval, self.hash).unwrap();
                        }

                        return eval;
                    }
                    transposition_table::MoveType::FailLow => {
                        if eval < alpha {
                            if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                                debug!(
                                    "Board hash of interest using evaluation from tt fail low: {} {:04x}",
                                    tt_data.important_move.pretty_print(Some(self)),
                                    tt_data.important_move.data
                                );
                            }

                            if let Some(e) = eval_tree_file {
                                writeln!(
                                    e,
                                    "r/{}/eval_low_tt {} hash {:#018x}",
                                    move_tree.join("/"),
                                    eval,
                                    self.hash
                                )
                                .unwrap();
                            }

                            return eval;
                        }
                    }
                }
            }

            self.make_move(&tt_data.important_move, rollback);
            move_tree.push(tt_data.important_move.simple_long_algebraic_notation());

            let result;
            if self.halfmove_clock >= 100
                || RepetitionTracker::test_threefold_repetition(self)
                || self.is_insufficient_material()
            {
                result = 0;
            } else {
                result = -self.alpha_beta_recurse(
                    -beta,
                    -alpha,
                    draft - 1,
                    ply + 1,
                    rollback,
                    stats,
                    transposition_table,
                    &mut new_killers,
                    history_table,
                    &mut line,
                    eval_tree_file,
                    move_tree,
                );
            }

            self.unmake_move(&tt_data.important_move, rollback);
            move_tree.pop();

            if result >= beta {
                self.update_killers_and_history(killers, &tt_data.important_move, history_table, ply);

                let mut tt_eval = result;
                if tt_eval >= MATE_THRESHOLD {
                    tt_eval += 10 * ply as i16;
                } else if tt_eval <= -MATE_THRESHOLD {
                    tt_eval -= 10 * ply as i16;
                }

                transposition_table.store_entry(
                    TTEntry {
                        hash: self.hash,
                        important_move: tt_data.important_move,
                        move_type: MoveType::FailHigh,
                        eval: tt_eval,
                        draft,
                        empty: false,
                    },
                    TableType::Main,
                );

                if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                    debug!(
                        "Board hash of interest had fail high from tt: {} {:04x}",
                        tt_data.important_move.pretty_print(Some(self)),
                        tt_data.important_move.data
                    );
                }

                if let Some(e) = eval_tree_file {
                    writeln!(
                        e,
                        "r/{}/eval_high {} hash {:#018x}",
                        move_tree.join("/"),
                        result,
                        self.hash
                    )
                    .unwrap();
                }

                return result;
            }

            if result > best_value {
                best_value = result;
                best_move = Some(tt_data.important_move);
                if result > alpha {
                    alpha = result;
                }

                if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                    debug!(
                        "Board hash of interest new best move from tt: {} {:04x}",
                        tt_data.important_move.pretty_print(Some(self)),
                        tt_data.important_move.data
                    );
                }

                line.push(tt_data.important_move);
                *pv = line.clone();
            }
        }

        let mut moves = self.generate_pseudo_legal_moves_with_history(history_table);
        let mut searched_quiet_moves = Vec::new();
        let mut found_legal_move = false;

        if killers[0] != EMPTY_MOVE {
            let mut unmatched_killers = if killers[1] != EMPTY_MOVE { 2 } else { 1 };

            for m in moves.iter_mut() {
                if m.m == killers[0] {
                    m.score = MOVE_SCORE_KILLER_1;

                    if unmatched_killers == 1 {
                        break;
                    } else {
                        unmatched_killers -= 1;
                    }
                } else if m.m == killers[1] {
                    m.score = MOVE_SCORE_KILLER_2;

                    if unmatched_killers == 1 {
                        break;
                    } else {
                        unmatched_killers -= 1;
                    }
                }
            }
        }

        moves.sort_unstable_by_key(|m| Reverse(m.score));

        for r#move in moves {
            if tt_entry.is_some_and(|v| v.important_move == r#move.m) {
                // Hash move is assumed to be legal
                found_legal_move = true;
                continue;
            }

            let (legal, move_made) = self.test_legality_and_maybe_make_move(r#move.m, rollback);
            if !legal {
                if move_made {
                    self.unmake_move(&r#move.m, rollback);
                }

                continue;
            } else {
                found_legal_move = true;
            }
            move_tree.push(r#move.m.simple_long_algebraic_notation());

            let result;
            if self.halfmove_clock >= 100
                || RepetitionTracker::test_threefold_repetition(self)
                || self.is_insufficient_material()
            {
                result = 0;
            } else {
                result = -self.alpha_beta_recurse(
                    -beta,
                    -alpha,
                    draft - 1,
                    ply + 1,
                    rollback,
                    stats,
                    transposition_table,
                    &mut new_killers,
                    history_table,
                    &mut line,
                    eval_tree_file,
                    move_tree,
                );
            }

            self.unmake_move(&r#move.m, rollback);
            move_tree.pop();

            if result >= beta {
                self.update_killers_and_history(killers, &r#move.m, history_table, ply);

                let penalty = -(ply as i16) * (ply as i16);
                for m in searched_quiet_moves {
                    self.update_history(history_table, &m, penalty);
                }

                let mut tt_eval = result;
                if tt_eval >= MATE_THRESHOLD {
                    tt_eval += 10 * ply as i16;
                } else if tt_eval <= -MATE_THRESHOLD {
                    tt_eval -= 10 * ply as i16;
                }

                transposition_table.store_entry(
                    TTEntry {
                        hash: self.hash,
                        important_move: r#move.m,
                        move_type: MoveType::FailHigh,
                        eval: tt_eval,
                        draft,
                        empty: false,
                    },
                    TableType::Main,
                );

                if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                    debug!(
                        "Board hash of interest had fail high from move gen: {} {:04x}",
                        r#move.m.pretty_print(Some(self)),
                        r#move.m.data
                    );
                }

                if let Some(e) = eval_tree_file {
                    writeln!(
                        e,
                        "r/{}/eval_high {} hash {:#018x}",
                        move_tree.join("/"),
                        result,
                        self.hash
                    )
                    .unwrap();
                }

                return result;
            }

            if result > best_value {
                best_value = result;
                best_move = Some(r#move.m);
                if result > alpha {
                    alpha = result;
                }

                if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                    debug!(
                        "Board hash of interest new best move from move gen: {} {:04x}",
                        r#move.m.pretty_print(Some(self)),
                        r#move.m.data
                    );
                }
                line.push(r#move.m);
                *pv = line.clone();
            }

            if r#move.m.flags() == 0 {
                searched_quiet_moves.push(r#move.m);
            }
        }

        // Assuming no bug with move generation...
        if !found_legal_move {
            self.white_to_move = !self.white_to_move;
            let is_check = self.can_capture_opponent_king(false);
            self.white_to_move = !self.white_to_move;

            if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                debug!("Board hash of interest generated no moves");
            }

            if is_check {
                let result = self.evaluate_checkmate_side_to_move_relative(ply);

                if let Some(e) = eval_tree_file {
                    writeln!(
                        e,
                        "r/{}/eval_mate {} hash {:#018x}",
                        move_tree.join("/"),
                        result,
                        self.hash
                    )
                    .unwrap();
                }

                return result;
            } else {
                if let Some(e) = eval_tree_file {
                    writeln!(e, "r/{}/eval_stalemate 0 hash {:#018x}", move_tree.join("/"), self.hash).unwrap();
                }

                return 0;
            }
        }

        let move_type = if alpha == best_value {
            MoveType::Best
        } else {
            MoveType::FailLow
        };

        if let Some(e) = eval_tree_file {
            writeln!(
                e,
                "r/{}/eval_inherit{} {} hash {:#018x}",
                move_tree.join("/"),
                if move_type == MoveType::FailLow { "_low" } else { "" },
                best_value,
                self.hash
            )
            .unwrap();
        }

        let mut tt_eval = best_value;
        if tt_eval >= MATE_THRESHOLD {
            tt_eval += 10 * ply as i16;
        } else if tt_eval <= -MATE_THRESHOLD {
            tt_eval -= 10 * ply as i16;
        }

        transposition_table.store_entry(
            TTEntry {
                hash: self.hash,
                important_move: best_move.unwrap(),
                move_type,
                eval: tt_eval,
                draft,
                empty: false,
            },
            TableType::Main,
        );

        best_value
    }

    pub fn quiescense_side_to_move_relative(
        &mut self,
        mut alpha: i16,
        beta: i16,
        ply: u8,
        rollback: &mut MoveRollback,
        stats: &mut SearchStats,
        transposition_table: &mut TranspositionTable,
    ) -> i16 {
        stats.quiescense_nodes += 1;

        if self.is_insufficient_material() {
            return 0;
        }

        let tt_entry = transposition_table.get_entry(self.hash, TableType::Quiescense);
        if let Some(tt_data) = tt_entry {
            match tt_data.move_type {
                transposition_table::MoveType::FailHigh => {
                    if tt_data.eval >= beta {
                        return tt_data.eval;
                    }
                }
                transposition_table::MoveType::Best => {
                    return tt_data.eval;
                }
                transposition_table::MoveType::FailLow => {
                    if tt_data.eval < alpha {
                        return tt_data.eval;
                    }
                }
            }
        }

        let stand_pat = self.evaluate_side_to_move_relative();

        if stand_pat >= beta {
            return stand_pat;
        }

        if self.game_stage > ENDGAME_GAME_STAGE_FOR_QUIESCENSE {
            // avoid underflow
            if alpha >= i16::MIN + 1000 && stand_pat < alpha - 1000 {
                stats.quiescense_cut_by_hopeless += 1;
                return alpha;
            }
        }

        if alpha < stand_pat {
            alpha = stand_pat;
        }

        let mut best_value = stand_pat;
        let mut best_move = None;

        if let Some(tt_data) = tt_entry {
            if tt_data.important_move.flags() & MOVE_FLAG_CAPTURE != 0 {
                self.make_move(&tt_data.important_move, rollback);

                // Only doing captures right now so not checking halfmove or threefold repetition here
                let result = -self.quiescense_side_to_move_relative(
                    -beta,
                    -alpha,
                    ply + 1,
                    rollback,
                    stats,
                    transposition_table,
                );

                self.unmake_move(&tt_data.important_move, rollback);

                if result >= beta {
                    transposition_table.store_entry(
                        TTEntry {
                            hash: self.hash,
                            important_move: tt_data.important_move,
                            move_type: MoveType::FailHigh,
                            eval: result,
                            draft: 0,
                            empty: false,
                        },
                        TableType::Quiescense,
                    );

                    return result;
                }

                if result > best_value {
                    best_value = result;
                    best_move = Some(tt_data.important_move);
                    if result > alpha {
                        alpha = result;
                    }
                }
            }
        }

        let mut moves = self.generate_pseudo_legal_capture_moves();

        moves.sort_unstable_by_key(|m| Reverse(m.score));

        for r#move in moves {
            let (legal, move_made) = self.test_legality_and_maybe_make_move(r#move.m, rollback);
            if !legal {
                if move_made {
                    self.unmake_move(&r#move.m, rollback);
                }

                continue;
            }

            // Only doing captures right now so not checking halfmove or threefold repetition here
            let result =
                -self.quiescense_side_to_move_relative(-beta, -alpha, ply + 1, rollback, stats, transposition_table);

            self.unmake_move(&r#move.m, rollback);

            if result >= beta {
                transposition_table.store_entry(
                    TTEntry {
                        hash: self.hash,
                        important_move: r#move.m,
                        move_type: MoveType::FailHigh,
                        eval: result,
                        draft: 0,
                        empty: false,
                    },
                    TableType::Quiescense,
                );

                return result;
            }

            if best_value < result {
                best_value = result;
                best_move = Some(r#move.m);

                if alpha < result {
                    alpha = result;
                }
            }
        }

        if let Some(bm) = best_move {
            transposition_table.store_entry(
                TTEntry {
                    hash: self.hash,
                    important_move: bm,
                    move_type: if alpha == best_value {
                        MoveType::Best
                    } else {
                        MoveType::FailLow
                    },
                    eval: best_value,
                    draft: 0,
                    empty: false,
                },
                TableType::Quiescense,
            );
        }

        best_value
    }

    #[inline]
    fn update_killers_and_history(
        &self,
        killers: &mut [Move; 2],
        m: &Move,
        history_table: &mut HistoryTable,
        ply_depth: u8,
    ) {
        if m.flags() != 0 {
            return;
        }

        self.update_history(history_table, m, (ply_depth as i16) * (ply_depth as i16));

        if killers[0] == *m {
            return;
        }

        if killers[1] == *m {
            (killers[0], killers[1]) = (killers[1], killers[0]);
        } else {
            killers[1] = *m;
        }
    }

    #[inline]
    fn update_history(&self, history_table: &mut HistoryTable, m: &Move, bonus: i16) {
        // from https://www.chessprogramming.org/History_Heuristic
        let piece_type = (self.get_piece_64(m.from() as usize) & PIECE_MASK) as usize;
        let history_color_value = if self.white_to_move { 0 } else { 1 };

        let current_value = &mut history_table[history_color_value][piece_type - 1][m.to() as usize];
        let clamped_bonus = (bonus as i32).clamp(-MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_HISTORY_MAX);
        *current_value +=
            (clamped_bonus - ((*current_value as i32) * clamped_bonus.abs() / MOVE_SCORE_HISTORY_MAX)) as i16;
    }
}
