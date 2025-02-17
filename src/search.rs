use std::fs;
use std::io::Write;
use std::{cmp::Reverse, fs::File, i16, path::PathBuf, sync::mpsc::Receiver, time::Instant};

use log::{debug, error};
use rand::random;
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_MASK},
    evaluate::{CENTIPAWN_VALUES, ENDGAME_GAME_STAGE_FOR_QUIESCENSE, MATE_THRESHOLD, MATE_VALUE},
    move_generator::{ScoredMove, MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_KILLER_1, MOVE_SCORE_KILLER_2},
    moves::{Move, MoveRollback, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_CAPTURE_FULL, MOVE_FLAG_PROMOTION},
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
    pub pv: Vec<Move>,
}

pub struct AlphaBetaResult {
    pub search_result: Option<SearchResult>,
    pub end_search: bool,
    pub stop_received: bool,
}

pub struct Searcher<'a> {
    board: &'a mut Board,
    rollback: MoveRollback,
    pub stats: SearchStats,
    transposition_table: &'a mut TranspositionTable,
    history_table: &'a mut HistoryTable,
}

impl<'a> Searcher<'a> {
    pub fn new(
        board: &'a mut Board,
        transposition_table: &'a mut TranspositionTable,
        history_table: &'a mut HistoryTable,
    ) -> Self {
        Self {
            board,
            rollback: MoveRollback::default(),
            stats: SearchStats::default(),
            transposition_table,
            history_table,
        }
    }

    pub fn iterative_deepening_search(
        &mut self,
        time: &Option<UciTimeControl>,
        search: &Option<UciSearchControl>,
        stop_rx: &Receiver<()>,
    ) -> SearchResult {
        let start_time = Instant::now();
        let mut target_dur = None;
        let search_control: SearchControl;
        let mut max_depth = 40;

        debug!("starting search of {}", self.board.to_fen());

        if let Some(t) = time {
            match t {
                UciTimeControl::TimeLeft {
                    white_time,
                    black_time,
                    white_increment,
                    black_increment,
                    moves_to_go,
                } => {
                    let time_left = if self.board.white_to_move {
                        white_time
                    } else {
                        black_time
                    };

                    if time_left.is_none() {
                        error!("No time left value provided when searching");
                        panic!("No time left value provided when searching");
                    }

                    let divisor = if self.board.fullmove_counter < 10 {
                        21
                    } else if self.board.fullmove_counter < 20 {
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
                    //     let eval = self.board.evaluate();
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
                        (self.board.fullmove_counter - 1) * 2 + if self.board.white_to_move { 0 } else { 1 },
                        self.board.to_fen().replace("/", "."),
                        depth
                    )))
                    .unwrap(),
                )
            } else {
                None
            };

            let result = self.alpha_beta_init(
                depth,
                &cancel_search_time,
                stop_rx,
                search_control == SearchControl::Infinite,
                &mut eval_tree_file,
            );
            if let Some(search_result) = result.search_result {
                let elapsed = start_time.elapsed();

                // print less when using TT values
                if self.stats.leaf_nodes != 0 || depth == 1 {
                    UciInterface::print_search_info(search_result.eval, &self.stats, &elapsed, &search_result.pv);
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
                // if search_result.self.stats.depth > depth {
                //     depth = search_result.self.stats.depth;
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
        cancel_search_at: &Option<Instant>,
        stop_rx: &Receiver<()>,
        infinite_search: bool,
        eval_tree_file: &mut Option<File>,
    ) -> AlphaBetaResult {
        let mut alpha = -i16::MAX;
        let mut best_value = -i16::MAX;
        let mut best_move = None;
        self.rollback = MoveRollback::default();
        self.stats = SearchStats::default();
        let mut killers = [EMPTY_MOVE, EMPTY_MOVE];
        let mut pv = Vec::new();
        let mut line = Vec::new();
        let mut move_tree = Vec::new();

        let mut moves;
        let tt_entry = self.transposition_table.get_entry(self.board.hash, TableType::Main);
        if let Some(tt_data) = tt_entry {
            // if tt_data.draft >= draft {
            //     if let self.transposition_table::MoveType::Best = tt_data.move_type {
            //         // should this be done for the root????
            //         self.stats.depth = tt_data.draft;
            //         return AlphaBetaResult {
            //             search_result: Some(SearchResult {
            //                 best_move: tt_data.important_move,
            //                 eval: tt_data.eval * if self.board.white_to_move { 1 } else { -1 },
            //                 self.stats,
            //             }),
            //             end_search: false,
            //         };
            //     }
            // }

            moves = Vec::from([ScoredMove {
                m: tt_data.important_move,
                score: 1,
            }]);
        } else {
            moves = Vec::new();
        }

        let mut legal_moves: u16 = 0;

        // Round 0 is the tt move, round 1 is regular move gen
        for round in 0..2 {
            for r#move in moves {
                if round == 1 && tt_entry.is_some_and(|v| v.important_move == r#move.m) {
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

                let (legal, move_made) = self
                    .board
                    .test_legality_and_maybe_make_move(r#move.m, &mut self.rollback);
                if !legal {
                    if move_made {
                        self.board.unmake_move(&r#move.m, &mut self.rollback);
                    }

                    continue;
                } else {
                    legal_moves += 1;
                }

                move_tree.push(r#move.m.simple_long_algebraic_notation());

                // let bonus = (random::<u16>() % 11) as i16 - 5;
                let bonus = 0;
                assert!(bonus > -6);
                assert!(bonus < 6);
                let score = -self.alpha_beta_recurse(
                    -i16::MAX,
                    -alpha,
                    draft - 1,
                    1,
                    &mut killers,
                    &mut line,
                    eval_tree_file,
                    &mut move_tree,
                );
                let result = bonus + score;
                debug!(
                    "{}: score {score} bonus {bonus} result {result}",
                    r#move.m.pretty_print(Some(self.board))
                );

                self.board.unmake_move(&r#move.m, &mut self.rollback);
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

            if round == 0 {
                moves = self.board.generate_pseudo_legal_moves_with_history(self.history_table);

                moves.sort_unstable_by_key(|m| Reverse(m.score));
            } else {
                moves = Vec::new();
            }
        }

        if legal_moves == 0 {
            error!(
                "Tried to search on a position but found no moves. Position: {:#?}",
                self.board
            );
            panic!("Tried to search on a position but found no moves");
        }

        if let Some(e) = eval_tree_file {
            writeln!(e, "eval_inherit {} hash {:#018x}", best_value, self.board.hash).unwrap();
        }

        self.stats.depth = draft;
        if legal_moves == 1 && !infinite_search {
            let bm = best_move.unwrap();

            return AlphaBetaResult {
                search_result: Some(SearchResult {
                    best_move: bm,
                    eval: self.board.evaluate(),
                    pv: [bm].to_vec(),
                }),
                end_search: true,
                stop_received: false,
            };
        }

        // Make the score not side-to-move relative
        AlphaBetaResult {
            search_result: Some(SearchResult {
                best_move: best_move.unwrap(),
                eval: best_value * if self.board.white_to_move { 1 } else { -1 },
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
        mut draft: u8,
        ply: u8,
        killers: &mut [Move; 2],
        pv: &mut Vec<Move>,
        eval_tree_file: &mut Option<File>,
        move_tree: &mut Vec<String>,
    ) -> i16 {
        if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.board.hash) {
            debug!("Board hash of interest found: {:#?}", self.board)
        }

        let is_pv = alpha + 1 == beta;

        if self.board.halfmove_clock >= 100
            || RepetitionTracker::test_threefold_repetition(self.board)
            || self.board.is_insufficient_material()
        {
            if is_pv {
                pv.clear();
            }

            return 0;
        }

        let in_check = self.board.can_capture_opponent_king(false);
        if in_check {
            draft += 1;
        }

        if draft == 0 {
            self.stats.leaf_nodes += 1;

            if is_pv {
                pv.clear();
            }

            let score = self.quiescense_side_to_move_relative(alpha, beta);

            if let Some(e) = eval_tree_file {
                writeln!(
                    e,
                    "r/{}/eval {} hash {:#018x}",
                    move_tree.join("/"),
                    score,
                    self.board.hash
                )
                .unwrap();
            }

            return score;
        }

        let mut best_value = -i16::MAX;
        let mut best_move = None;
        let mut new_killers = [EMPTY_MOVE, EMPTY_MOVE];
        let mut line = Vec::new();

        let mut moves;
        let tt_entry = self.transposition_table.get_entry(self.board.hash, TableType::Main);
        if let Some(tt_data) = tt_entry {
            if !is_pv && tt_data.draft >= draft {
                let eval = tt_data.get_eval(ply);

                match tt_data.move_type {
                    transposition_table::MoveType::FailHigh => {
                        if eval >= beta {
                            self.update_killers_and_history(killers, &tt_data.important_move, ply);

                            if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.board.hash) {
                                debug!(
                                    "Board hash of interest using evaluation from tt fail high: {} {:04x}",
                                    tt_data.important_move.pretty_print(Some(self.board)),
                                    tt_data.important_move.data
                                );
                            }

                            if let Some(e) = eval_tree_file {
                                writeln!(
                                    e,
                                    "r/{}/eval_high_tt {} hash {:#018x}",
                                    move_tree.join("/"),
                                    eval,
                                    self.board.hash
                                )
                                .unwrap();
                            }

                            return eval;
                        }
                    }
                    transposition_table::MoveType::Best => {
                        if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.board.hash) {
                            debug!(
                                "Board hash of interest using evaluation from tt best move: {} {:04x}",
                                tt_data.important_move.pretty_print(Some(self.board)),
                                tt_data.important_move.data
                            );
                        }

                        if let Some(e) = eval_tree_file {
                            writeln!(
                                e,
                                "r/{}/eval_tt {} hash {:#018x}",
                                move_tree.join("/"),
                                eval,
                                self.board.hash
                            )
                            .unwrap();
                        }

                        return eval;
                    }
                    transposition_table::MoveType::FailLow => {
                        if eval < alpha {
                            if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.board.hash) {
                                debug!(
                                    "Board hash of interest using evaluation from tt fail low: {} {:04x}",
                                    tt_data.important_move.pretty_print(Some(self.board)),
                                    tt_data.important_move.data
                                );
                            }

                            if let Some(e) = eval_tree_file {
                                writeln!(
                                    e,
                                    "r/{}/eval_low_tt {} hash {:#018x}",
                                    move_tree.join("/"),
                                    eval,
                                    self.board.hash
                                )
                                .unwrap();
                            }

                            return eval;
                        }
                    }
                }
            }

            moves = Vec::from([ScoredMove {
                m: tt_data.important_move,
                score: 1,
            }]);
        } else {
            moves = Vec::new();
        }

        let mut searched_quiet_moves = Vec::new();
        let mut found_legal_move = false;
        let mut searched_moves = 0;

        // Round 0 is the tt move, round 1 is regular move gen
        for round in 0..2 {
            for r#move in moves {
                if round == 1 && tt_entry.is_some_and(|v| v.important_move == r#move.m) {
                    continue;
                }

                let (legal, move_made) = self
                    .board
                    .test_legality_and_maybe_make_move(r#move.m, &mut self.rollback);
                if !legal {
                    if move_made {
                        self.board.unmake_move(&r#move.m, &mut self.rollback);
                    }

                    continue;
                } else {
                    found_legal_move = true;
                }

                move_tree.push(r#move.m.simple_long_algebraic_notation());

                let mut reduction = 0;
                // Late move reduction
                if draft > 2 && searched_moves > 3 {
                    let flags = r#move.m.flags();

                    // Using formula and values from Ethereal according to https://www.chessprogramming.org/Late_Move_Reductions
                    reduction = if flags & MOVE_FLAG_CAPTURE == 0 && flags & MOVE_FLAG_PROMOTION == 0 {
                        (0.7844 + (draft as f32).ln() * (searched_moves as f32).ln() / 2.4696).round() as u8
                    } else {
                        3
                    };

                    if is_pv {
                        reduction -= 1;
                    }

                    if in_check {
                        reduction -= 1;
                    }

                    reduction = reduction.clamp(0, draft - 1)
                }

                let mut result;
                if searched_moves == 0 {
                    result = -self.alpha_beta_recurse(
                        -beta,
                        -alpha,
                        draft - reduction - 1,
                        ply + 1,
                        &mut new_killers,
                        &mut line,
                        eval_tree_file,
                        move_tree,
                    );

                    if result > alpha && reduction > 0 {
                        line.clear();
                        result = -self.alpha_beta_recurse(
                            -beta,
                            -alpha,
                            draft - 1,
                            ply + 1,
                            &mut new_killers,
                            &mut line,
                            eval_tree_file,
                            move_tree,
                        );
                    }
                } else {
                    result = -self.alpha_beta_recurse(
                        -alpha - 1,
                        -alpha,
                        draft - reduction - 1,
                        ply + 1,
                        &mut new_killers,
                        &mut line,
                        eval_tree_file,
                        move_tree,
                    );

                    // if result > alpha && reduction > 0 {
                    //     result = -self.alpha_beta_recurse(-alpha - 1, -alpha, draft - 1, ply + 1, &mut new_killers);
                    // }

                    if result > alpha {
                        result = -self.alpha_beta_recurse(
                            -beta,
                            -alpha,
                            draft - 1,
                            ply + 1,
                            &mut new_killers,
                            &mut line,
                            eval_tree_file,
                            move_tree,
                        );
                    }
                }

                self.board.unmake_move(&r#move.m, &mut self.rollback);
                searched_moves += 1;
                move_tree.pop();

                if result >= beta {
                    self.update_killers_and_history(killers, &r#move.m, ply);

                    let penalty = -(ply as i16) * (ply as i16);
                    for m in searched_quiet_moves {
                        self.update_history(&m, penalty);
                    }

                    self.transposition_table.store_entry(
                        TTEntry::new(self.board.hash, r#move.m, MoveType::FailHigh, result, draft, ply),
                        TableType::Main,
                    );

                    if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.board.hash) {
                        debug!(
                            "Board hash of interest had fail high from {}: {} {:04x}",
                            if round == 0 { "tt" } else { "move gen" },
                            r#move.m.pretty_print(Some(self.board)),
                            r#move.m.data
                        );
                    }

                    if let Some(e) = eval_tree_file {
                        writeln!(
                            e,
                            "r/{}/eval_high {} hash {:#018x}",
                            move_tree.join("/"),
                            result,
                            self.board.hash
                        )
                        .unwrap();
                    }

                    if is_pv {
                        line.push(r#move.m);
                        *pv = line.clone();
                    }

                    return result;
                }

                if result > best_value {
                    best_value = result;
                    best_move = Some(r#move.m);
                    if result > alpha {
                        alpha = result;
                    }

                    if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.board.hash) {
                        debug!(
                            "Board hash of interest got new best move from {}: {} {:04x}",
                            if round == 0 { "tt" } else { "move gen" },
                            r#move.m.pretty_print(Some(self.board)),
                            r#move.m.data
                        );
                    }

                    if is_pv {
                        line.push(r#move.m);
                        *pv = line.clone();
                    }
                }

                if r#move.m.flags() == 0 {
                    searched_quiet_moves.push(r#move.m);
                }
            }

            if round == 0 {
                moves = self.board.generate_pseudo_legal_moves_with_history(&self.history_table);

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
            } else {
                moves = Vec::new();
            }
        }

        // Assuming no bug with move generation...
        if !found_legal_move {
            self.board.white_to_move = !self.board.white_to_move;
            let is_check = self.board.can_capture_opponent_king(false);
            self.board.white_to_move = !self.board.white_to_move;

            if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.board.hash) {
                debug!("Board hash of interest generated no moves");
            }

            if is_check {
                let result = self.board.evaluate_checkmate_side_to_move_relative(ply);

                if let Some(e) = eval_tree_file {
                    writeln!(
                        e,
                        "r/{}/eval_mate {} hash {:#018x}",
                        move_tree.join("/"),
                        result,
                        self.board.hash
                    )
                    .unwrap();
                }

                if is_pv {
                    pv.clear();
                }

                return result;
            } else {
                if let Some(e) = eval_tree_file {
                    writeln!(
                        e,
                        "r/{}/eval_stalemate 0 hash {:#018x}",
                        move_tree.join("/"),
                        self.board.hash
                    )
                    .unwrap();
                }

                if is_pv {
                    pv.clear();
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
                self.board.hash
            )
            .unwrap();
        }

        self.transposition_table.store_entry(
            TTEntry::new(self.board.hash, best_move.unwrap(), move_type, best_value, draft, ply),
            TableType::Main,
        );

        best_value
    }

    pub fn quiescense_side_to_move_relative(&mut self, mut alpha: i16, beta: i16) -> i16 {
        self.stats.quiescense_nodes += 1;

        if self.board.is_insufficient_material() {
            return 0;
        }

        let mut moves;
        let tt_entry = self
            .transposition_table
            .get_entry(self.board.hash, TableType::Quiescense);
        if let Some(tt_data) = tt_entry {
            let tt_eval = tt_data.get_eval(0);

            match tt_data.move_type {
                transposition_table::MoveType::FailHigh => {
                    if tt_eval >= beta {
                        return tt_eval;
                    }
                }
                transposition_table::MoveType::Best => {
                    return tt_eval;
                }
                transposition_table::MoveType::FailLow => {
                    if tt_eval < alpha {
                        return tt_eval;
                    }
                }
            }

            moves = Vec::from([ScoredMove {
                m: tt_data.important_move,
                score: 1,
            }]);
        } else {
            moves = Vec::new();
        }

        let stand_pat = self.board.evaluate_side_to_move_relative();

        if stand_pat >= beta {
            return stand_pat;
        }

        if self.board.game_stage > ENDGAME_GAME_STAGE_FOR_QUIESCENSE {
            // avoid underflow
            if alpha >= i16::MIN + 1000 && stand_pat < alpha - 1000 {
                self.stats.quiescense_cut_by_hopeless += 1;
                return alpha;
            }
        }

        if alpha < stand_pat {
            alpha = stand_pat;
        }

        let mut best_value = stand_pat;
        let mut best_move = None;

        // Round 0 is the tt move, round 1 is regular move gen
        for round in 0..2 {
            for r#move in moves {
                if round == 1 && tt_entry.is_some_and(|v| v.important_move == r#move.m) {
                    continue;
                }

                let (legal, move_made) = self
                    .board
                    .test_legality_and_maybe_make_move(r#move.m, &mut self.rollback);
                if !legal {
                    if move_made {
                        self.board.unmake_move(&r#move.m, &mut self.rollback);
                    }

                    continue;
                }

                // Only doing captures right now so not checking halfmove or threefold repetition here
                let result = -self.quiescense_side_to_move_relative(-beta, -alpha);

                self.board.unmake_move(&r#move.m, &mut self.rollback);

                if result >= beta {
                    self.transposition_table.store_entry(
                        TTEntry::new(self.board.hash, r#move.m, MoveType::FailHigh, result, 0, 0),
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

            if round == 0 {
                moves = self.board.generate_pseudo_legal_capture_moves();

                moves.sort_unstable_by_key(|m| Reverse(m.score));
            } else {
                moves = Vec::new();
            }
        }

        if let Some(bm) = best_move {
            let entry_type = if alpha == best_value {
                MoveType::Best
            } else {
                MoveType::FailLow
            };
            self.transposition_table.store_entry(
                TTEntry::new(self.board.hash, bm, entry_type, best_value, 0, 0),
                TableType::Quiescense,
            );
        }

        best_value
    }

    #[inline]
    fn update_killers_and_history(&mut self, killers: &mut [Move; 2], m: &Move, ply_depth: u8) {
        if m.flags() != 0 {
            return;
        }

        self.update_history(m, (ply_depth as i16) * (ply_depth as i16));

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
    fn update_history(&mut self, m: &Move, bonus: i16) {
        // from https://www.chessprogramming.org/History_Heuristic
        let piece_type = (self.board.get_piece_64(m.from() as usize) & PIECE_MASK) as usize;
        let history_color_value = if self.board.white_to_move { 0 } else { 1 };

        let current_value = &mut self.history_table[history_color_value][piece_type - 1][m.to() as usize];
        let clamped_bonus = (bonus as i32).clamp(-MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_HISTORY_MAX);
        *current_value +=
            (clamped_bonus - ((*current_value as i32) * clamped_bonus.abs() / MOVE_SCORE_HISTORY_MAX)) as i16;
    }
}
