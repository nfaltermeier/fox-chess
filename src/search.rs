use std::{
    cmp::{Ordering, Reverse},
    i16, iter,
    time::Instant,
};

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

// pub const TEST_TT_FOR_HASH_COLLISION: bool = true;

#[derive(Default)]
pub struct SearchStats {
    pub quiescense_nodes: u64,
    pub depth: u8,
    pub quiescense_cut_by_hopeless: u64,
    pub leaf_nodes: u64,
}

pub struct SearchResult {
    pub best_move: Move,
    pub eval: i16,
}

pub struct AlphaBetaResult {
    pub search_result: Option<SearchResult>,
    pub end_search: bool,
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
    ) -> SearchResult {
        let start_time = Instant::now();
        let target_dur;

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
                    target_dur = time_left
                        .as_ref()
                        .unwrap()
                        .to_std()
                        .unwrap()
                        .checked_div(divisor)
                        .unwrap();

                    // maybe something like https://www.desmos.com/calculator/47t9iys2fo
                    // let expected_moves_left = if let Some(mtg) = moves_to_go {
                    //     *mtg
                    // } else {
                    //     let eval = self.board.evaluate();
                    // }
                }
                UciTimeControl::MoveTime(time_delta) => {
                    target_dur = time_delta.to_std().unwrap();
                }
                UciTimeControl::Ponder => {
                    unimplemented!("uci go ponder");
                }
                UciTimeControl::Infinite => {
                    unimplemented!("uci go infinite");
                }
            }
        } else {
            error!("UCI time control not passed to go, currently required");
            unimplemented!("UCI time control not passed to go, currently required");
        }

        // Above 5 depth moves can start taking a lot more time
        let cutoff_low_depth = target_dur.mul_f32(0.55);
        let cutoff = target_dur.mul_f32(0.35);
        let cancel_search_time = start_time.checked_add(target_dur.mul_f32(2.0)).unwrap();

        let mut depth = 1;
        let mut latest_result = None;
        loop {
            let result = self.alpha_beta_init(depth, &cancel_search_time);
            if let Some(search_result) = result.search_result {
                let elapsed = start_time.elapsed();

                // print less when using TT values
                if self.stats.leaf_nodes != 0 || depth == 1 {
                    UciInterface::print_search_info(search_result.eval, &self.stats, &elapsed);
                }

                if result.end_search
                    || (depth >= 5 && elapsed >= cutoff)
                    || elapsed >= cutoff_low_depth
                    || search_result.eval.abs() >= MATE_THRESHOLD
                    || depth >= 40
                {
                    return search_result;
                }

                // This seems like it could go really wrong. For when entire search is skipped by tt best move node.
                // if search_result.self.stats.depth > depth {
                //     depth = search_result.self.stats.depth;
                // }

                latest_result = Some(search_result);
            } else {
                debug!("Cancelled search of depth {depth} due to exceeding time budget");
                return latest_result
                    .expect("iterative_deepening_search exceeded cancel_search_time before completing any searches");
            }

            depth += 1;
        }
    }

    pub fn alpha_beta_init(&mut self, draft: u8, cancel_search_at: &Instant) -> AlphaBetaResult {
        let mut alpha = -i16::MAX;
        let mut best_value = -i16::MAX;
        let mut best_move = None;
        self.rollback = MoveRollback::default();
        self.stats = SearchStats::default();
        let mut killers = [EMPTY_MOVE, EMPTY_MOVE];

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

                if Instant::now() >= *cancel_search_at {
                    return AlphaBetaResult {
                        search_result: None,
                        end_search: true,
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

                let result =
                    (random::<i16>() % 11) - 5 - self.alpha_beta_recurse(-i16::MAX, -alpha, draft - 1, 1, &mut killers);

                self.board.unmake_move(&r#move.m, &mut self.rollback);

                if result > best_value {
                    best_value = result;
                    best_move = Some(r#move.m);
                    if result > alpha {
                        alpha = result;
                    }
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

        self.stats.depth = draft;
        if legal_moves == 1 {
            return AlphaBetaResult {
                search_result: Some(SearchResult {
                    best_move: best_move.unwrap(),
                    eval: self.board.evaluate(),
                }),
                end_search: true,
            };
        }

        // Make the score not side-to-move relative
        AlphaBetaResult {
            search_result: Some(SearchResult {
                best_move: best_move.unwrap(),
                eval: best_value * if self.board.white_to_move { 1 } else { -1 },
            }),
            end_search: false,
        }
    }

    fn alpha_beta_recurse(
        &mut self,
        mut alpha: i16,
        beta: i16,
        mut draft: u8,
        ply: u8,
        killers: &mut [Move; 2],
    ) -> i16 {
        if self.board.halfmove_clock >= 100
            || RepetitionTracker::test_threefold_repetition(self.board)
            || self.board.is_insufficient_material()
        {
            return 0;
        }

        let in_check = self.board.can_capture_opponent_king(false);
        if in_check {
            draft += 1;
        }

        if draft == 0 {
            self.stats.leaf_nodes += 1;
            return self.quiescense_side_to_move_relative(alpha, beta);
        }

        let mut best_value = -i16::MAX;
        let mut best_move = None;
        let mut new_killers = [EMPTY_MOVE, EMPTY_MOVE];

        let is_pv = alpha + 1 == beta;

        let mut moves;
        let tt_entry = self.transposition_table.get_entry(self.board.hash, TableType::Main);
        if let Some(tt_data) = tt_entry {
            if !is_pv && tt_data.draft >= draft {
                let eval = tt_data.get_eval(ply);

                match tt_data.move_type {
                    transposition_table::MoveType::FailHigh => {
                        if eval >= beta {
                            self.update_killers_and_history(killers, &tt_data.important_move, ply);

                            return eval;
                        }
                    }
                    transposition_table::MoveType::Best => {
                        return eval;
                    }
                    transposition_table::MoveType::FailLow => {
                        if eval < alpha {
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
                    result = -self.alpha_beta_recurse(-beta, -alpha, draft - reduction - 1, ply + 1, &mut new_killers);

                    if result > alpha && reduction > 0 {
                        result = -self.alpha_beta_recurse(-beta, -alpha, draft - 1, ply + 1, &mut new_killers);
                    }
                } else {
                    result =
                        -self.alpha_beta_recurse(-alpha - 1, -alpha, draft - reduction - 1, ply + 1, &mut new_killers);

                    // if result > alpha && reduction > 0 {
                    //     result = -self.alpha_beta_recurse(-alpha - 1, -alpha, draft - 1, ply + 1, &mut new_killers);
                    // }

                    if result > alpha {
                        result = -self.alpha_beta_recurse(-beta, -alpha, draft - 1, ply + 1, &mut new_killers);
                    }
                }

                self.board.unmake_move(&r#move.m, &mut self.rollback);
                searched_moves += 1;

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

                    return result;
                }

                if result > best_value {
                    best_value = result;
                    best_move = Some(r#move.m);
                    if result > alpha {
                        alpha = result;
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

            if is_check {
                return self.board.evaluate_checkmate_side_to_move_relative(ply);
            } else {
                return 0;
            }
        }

        let entry_type = if alpha == best_value {
            MoveType::Best
        } else {
            MoveType::FailLow
        };
        self.transposition_table.store_entry(
            TTEntry::new(self.board.hash, best_move.unwrap(), entry_type, best_value, draft, ply),
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
