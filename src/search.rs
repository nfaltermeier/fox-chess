use std::{
    cmp::{Ordering, Reverse}, collections::HashSet, i16, time::Instant
};

use log::{debug, error, trace};
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_MASK},
    evaluate::{CENTIPAWN_VALUES, ENDGAME_GAME_STAGE_FOR_QUIESCENSE},
    move_generator::{
        can_capture_opponent_king, generate_capture_moves, generate_moves_with_history, ScoredMove,
        MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_KILLER_1, MOVE_SCORE_KILLER_2,
    },
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
    pub pv: Vec<Move>,
}

enum SearchControl {
    Time,
    Depth,
}

pub struct SearchResult {
    pub best_move: Move,
    pub eval: i16,
    pub stats: SearchStats,
}

pub struct AlphaBetaResult {
    pub search_result: Option<SearchResult>,
    pub end_search: bool,
}

impl Board {
    pub fn iterative_deepening_search(
        &mut self,
        time: &Option<UciTimeControl>,
        search: &Option<UciSearchControl>,
        transposition_table: &mut TranspositionTable,
        history_table: &mut HistoryTable,
    ) -> SearchResult {
        let start_time = Instant::now();
        let mut target_dur = None;
        let search_control: SearchControl;
        let mut max_depth = 40;

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
                }
                UciTimeControl::MoveTime(time_delta) => {
                    target_dur = Some(time_delta.to_std().unwrap());
                }
                UciTimeControl::Ponder => {
                    unimplemented!("uci go ponder");
                }
                UciTimeControl::Infinite => {
                    unimplemented!("uci go infinite");
                }
            }
            search_control = SearchControl::Time;
        } else if let Some(s) = search {
            if s.depth.is_some() {
                search_control = SearchControl::Depth;
                max_depth = s.depth.unwrap();
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
                cancel_search_time  = Some(start_time.checked_add(d.mul_f32(2.0)).unwrap());
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
            let result = self.alpha_beta_init(depth, transposition_table, history_table, &cancel_search_time);
            if let Some(search_result) = result.search_result {
                let elapsed = start_time.elapsed();

                // print less when using TT values
                if search_result.stats.leaf_nodes != 0 || depth == 1 {
                    UciInterface::print_search_info(search_result.eval, &search_result.stats, &elapsed);
                }

                if result.end_search
                    || search_result.eval.abs() >= 19800
                    || depth >= max_depth
                    || match search_control {
                        SearchControl::Time => {
                            (depth >= 5 && elapsed >= cutoff.unwrap()) || elapsed >= cutoff_low_depth.unwrap()
                        }
                        SearchControl::Depth => false,
                    }
                {
                    return search_result;
                }

                latest_result = Some(search_result);
            } else {
                debug!("Cancelled search of depth {depth} due to exceeding time budget");
                return latest_result
                    .expect("iterative_deepening_search exceeded cancel_search_time before completing any searches");
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
    ) -> AlphaBetaResult {
        let mut alpha = -i16::MAX;
        let mut best_value = -i16::MAX;
        let mut best_move = None;
        let mut rollback = MoveRollback::default();
        let mut stats = SearchStats::default();
        let mut killers = [EMPTY_MOVE, EMPTY_MOVE];

        let tt_entry = transposition_table.get_entry(self.hash, TableType::Main);
        if let Some(tt_data) = tt_entry {
            if tt_data.move_num >= self.fullmove_counter + draft as u16 {
                if let transposition_table::MoveType::Best = tt_data.move_type {
                    self.gather_pv(&tt_data.important_move, transposition_table, &mut stats, &mut rollback);

                    // should this be done for the root????
                    return AlphaBetaResult {
                        search_result: Some(SearchResult {
                            best_move: tt_data.important_move,
                            eval: tt_data.eval * if self.white_to_move { 1 } else { -1 },
                            stats,
                        }),
                        end_search: false,
                    };
                }
            }

            self.make_move(&tt_data.important_move, &mut rollback);

            let result;
            if self.halfmove_clock >= 100 || RepetitionTracker::test_threefold_repetition(self) {
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
                );
            }

            self.unmake_move(&tt_data.important_move, &mut rollback);

            if result > best_value {
                best_value = result;
                best_move = Some(tt_data.important_move);
                if result > alpha {
                    alpha = result;
                }
            }
        }

        let mut moves = generate_moves_with_history(self, history_table);

        if moves.is_empty() {
            error!(
                "Tried to search on a position but found no moves. Position: {:#?}",
                self
            );
            panic!("Tried to search on a position but found no moves");
        }

        if moves.len() == 1 {
            stats.depth = 1;

            let m = moves.pop().unwrap().m;
            self.gather_pv(&m, transposition_table, &mut stats, &mut rollback);

            return AlphaBetaResult {
                search_result: Some(SearchResult {
                    best_move: m,
                    eval: self.evaluate(),
                    stats,
                }),
                end_search: true,
            };
        } else {
            stats.depth = draft;
        }

        moves.sort_by_key(|m| Reverse(m.score));

        for r#move in moves {
            if tt_entry.is_some_and(|v| v.important_move == r#move.m) {
                continue;
            }

            if cancel_search_at.is_some_and(|t| Instant::now() >= t) {
                return AlphaBetaResult {
                    search_result: None,
                    end_search: true,
                };
            }

            self.make_move(&r#move.m, &mut rollback);

            let result;
            if self.halfmove_clock >= 100 || RepetitionTracker::test_threefold_repetition(self) {
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
                );
            }

            self.unmake_move(&r#move.m, &mut rollback);

            if result > best_value {
                best_value = result;
                best_move = Some(r#move.m);
                if result > alpha {
                    alpha = result;
                }
            }
        }

        self.gather_pv(&best_move.unwrap(), transposition_table, &mut stats, &mut rollback);

        // Make the score not side-to-move relative
        AlphaBetaResult {
            search_result: Some(SearchResult {
                best_move: best_move.unwrap(),
                eval: best_value * if self.white_to_move { 1 } else { -1 },
                stats,
            }),
            end_search: false,
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
    ) -> i16 {
        if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
            debug!("Board hash of interest found: {self:#?}")
        }

        if draft == 0 {
            stats.leaf_nodes += 1;
            return self.quiescense_side_to_move_relative(alpha, beta, ply + 1, rollback, stats, transposition_table);
        }

        let mut best_value = -i16::MAX;
        let mut best_move = None;
        let mut new_killers = [EMPTY_MOVE, EMPTY_MOVE];

        let tt_entry = transposition_table.get_entry(self.hash, TableType::Main);
        if let Some(tt_data) = tt_entry {
            if tt_data.move_num >= self.fullmove_counter + draft as u16 {
                match tt_data.move_type {
                    transposition_table::MoveType::FailHigh => {
                        if tt_data.eval >= beta {
                            self.update_killers_and_history(killers, &tt_data.important_move, history_table, ply);

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

            self.make_move(&tt_data.important_move, rollback);

            let result;
            if self.halfmove_clock >= 100 || RepetitionTracker::test_threefold_repetition(self) {
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
                );
            }

            self.unmake_move(&tt_data.important_move, rollback);

            if result >= beta {
                self.update_killers_and_history(killers, &tt_data.important_move, history_table, ply);

                transposition_table.store_entry(
                    TTEntry {
                        hash: self.hash,
                        important_move: tt_data.important_move,
                        move_type: MoveType::FailHigh,
                        eval: result,
                        move_num: self.fullmove_counter + draft as u16,
                    },
                    TableType::Main,
                );

                if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                    debug!("Board hash of interest had fail high from tt: {} {:04x}", tt_data.important_move.pretty_print(Some(self)), tt_data.important_move.data);
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
                    debug!("Board hash of interest new best move from tt: {} {:04x}", tt_data.important_move.pretty_print(Some(self)), tt_data.important_move.data);
                }
            }
        }

        let mut moves = generate_moves_with_history(self, history_table);
        let mut searched_quiet_moves = Vec::new();

        // Assuming no bug with move generation...
        if moves.is_empty() {
            self.white_to_move = !self.white_to_move;
            let is_check = can_capture_opponent_king(self, false);
            self.white_to_move = !self.white_to_move;

            if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                debug!("Board hash of interest generated no moves");
            }

            if is_check {
                return self.evaluate_checkmate_side_to_move_relative(ply);
            } else {
                return 0;
            }
        }

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

        moves.sort_by_key(|m| Reverse(m.score));

        for r#move in moves {
            if tt_entry.is_some_and(|v| v.important_move == r#move.m) {
                continue;
            }

            self.make_move(&r#move.m, rollback);

            let result;
            if self.halfmove_clock >= 100 || RepetitionTracker::test_threefold_repetition(self) {
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
                );
            }

            self.unmake_move(&r#move.m, rollback);

            if result >= beta {
                self.update_killers_and_history(killers, &r#move.m, history_table, ply);

                let penalty = -(ply as i16) * (ply as i16);
                for m in searched_quiet_moves {
                    self.update_history(history_table, &m, penalty);
                }

                transposition_table.store_entry(
                    TTEntry {
                        hash: self.hash,
                        important_move: r#move.m,
                        move_type: MoveType::FailHigh,
                        eval: result,
                        move_num: self.fullmove_counter + draft as u16,
                    },
                    TableType::Main,
                );

                if DEBUG_BOARD_HASH_OF_INTEREST.is_some_and(|h| h == self.hash) {
                    debug!("Board hash of interest had fail high from move gen: {} {:04x}", r#move.m.pretty_print(Some(self)), r#move.m.data);
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
                    debug!("Board hash of interest new best move from move gen: {} {:04x}", r#move.m.pretty_print(Some(self)), r#move.m.data);
                }
            }

            if r#move.m.flags() == 0 {
                searched_quiet_moves.push(r#move.m);
            }
        }

        transposition_table.store_entry(
            TTEntry {
                hash: self.hash,
                important_move: best_move.unwrap(),
                move_type: if alpha == best_value {
                    MoveType::Best
                } else {
                    MoveType::FailLow
                },
                eval: best_value,
                move_num: self.fullmove_counter + draft as u16,
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

        let tt_entry = transposition_table.get_entry(self.hash, TableType::Quiescense);
        if let Some(tt_data) = tt_entry {
            if tt_data.move_num >= self.fullmove_counter {
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
                            move_num: self.fullmove_counter,
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

        let mut moves = generate_capture_moves(self);

        moves.sort_by_key(|m| Reverse(m.score));

        for r#move in moves {
            self.make_move(&r#move.m, rollback);
            let result;

            // Only doing captures right now so not checking halfmove or threefold repetition here
            result =
                -self.quiescense_side_to_move_relative(-beta, -alpha, ply + 1, rollback, stats, transposition_table);

            self.unmake_move(&r#move.m, rollback);

            if result >= beta {
                transposition_table.store_entry(
                    TTEntry {
                        hash: self.hash,
                        important_move: r#move.m,
                        move_type: MoveType::FailHigh,
                        eval: result,
                        move_num: self.fullmove_counter,
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
                    move_num: self.fullmove_counter,
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
        *current_value += (clamped_bonus - ((*current_value as i32) * clamped_bonus.abs() / MOVE_SCORE_HISTORY_MAX)) as i16;
    }

    fn gather_pv(
        &mut self,
        first_move: &Move,
        transposition_table: &mut TranspositionTable,
        stats: &mut SearchStats,
        rollback: &mut MoveRollback,
    ) {
        stats.pv.clear();

        // Prevent cycles from occurring
        let mut previous_hashes = HashSet::new();
        previous_hashes.insert(self.hash);
        stats.pv.push(*first_move);
        self.make_move(first_move, rollback);

        let mut next_move = transposition_table.get_entry(self.hash, TableType::Main);
        loop {
            match next_move {
                None => {
                    break;
                }
                Some(e) => {
                    if e.move_type != MoveType::Best || previous_hashes.contains(&self.hash) {
                        break;
                    }

                    previous_hashes.insert(self.hash);
                    stats.pv.push(e.important_move);
                    // trace!("gather_pv about to make move {}", e.important_move.pretty_print(Some(self)));
                    self.make_move(&e.important_move, rollback);
                    next_move = transposition_table.get_entry(self.hash, TableType::Main);
                }
            }
        }

        for m in stats.pv.iter().rev() {
            self.unmake_move(&m, rollback);
        }
    }
}
