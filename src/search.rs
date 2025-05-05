use std::{
    cmp::{Ordering, Reverse},
    i16, iter,
    time::{Duration, Instant},
};

use log::{debug, error};
use rand::random;
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_KING, PIECE_MASK, PIECE_PAWN},
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
    pub total_search_leaves: u64,
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
    starting_halfmove: u8,
    cancel_search_at: Option<Instant>,
}

impl<'a> Searcher<'a> {
    pub fn new(
        board: &'a mut Board,
        transposition_table: &'a mut TranspositionTable,
        history_table: &'a mut HistoryTable,
    ) -> Self {
        let starting_halfmove = board.halfmove_clock;

        Self {
            board,
            rollback: MoveRollback::default(),
            stats: SearchStats::default(),
            transposition_table,
            history_table,
            starting_halfmove,
            cancel_search_at: None,
        }
    }

    pub fn iterative_deepening_search(
        &mut self,
        time: &Option<UciTimeControl>,
        search: &Option<UciSearchControl>,
    ) -> SearchResult {
        let start_time = Instant::now();
        let mut target_dur = None;
        let search_control: SearchControl;
        let mut max_depth = 40;
        let use_stricter_cutoff;

        if let Some(t) = time {
            match t {
                UciTimeControl::TimeLeft {
                    white_time,
                    black_time,
                    white_increment,
                    black_increment,
                    moves_to_go,
                } => {
                    let (time_left, increment) = if self.board.white_to_move {
                        (white_time, white_increment)
                    } else {
                        (black_time, black_increment)
                    };

                    if time_left.is_none() {
                        error!("No time left value provided when searching");
                        panic!("No time left value provided when searching");
                    }

                    let divisor = if self.board.fullmove_counter < 15 {
                        35
                    } else if self.board.fullmove_counter < 25 {
                        30
                    } else {
                        40
                    };

                    let time_left = time_left
                        .as_ref()
                        .unwrap()
                        .to_std()
                        .unwrap();
                    target_dur = Some(time_left.checked_div(divisor).unwrap());

                    if let Some(inc) = increment {
                        let inc = inc.to_std().unwrap();

                        target_dur = Some(target_dur.unwrap().saturating_add(inc.mul_f32(0.7)));

                        use_stricter_cutoff = inc.abs_diff(time_left) < inc;
                    } else {
                        use_stricter_cutoff = time_left < Duration::from_secs(5);
                    }

                    search_control = SearchControl::Time;
                }
                UciTimeControl::MoveTime(time_delta) => {
                    target_dur = Some(time_delta.to_std().unwrap());
                    search_control = SearchControl::Time;
                    use_stricter_cutoff = true;
                }
                UciTimeControl::Ponder => {
                    unimplemented!("uci go ponder");
                }
                UciTimeControl::Infinite => {
                    // Need to copy 'stop' command functionality over from full-pv branch
                    unimplemented!("uci go infinite");
                    search_control = SearchControl::Infinite;
                }
            }
        } else if let Some(s) = search {
            if s.depth.is_some() {
                search_control = SearchControl::Depth;
                max_depth = s.depth.unwrap();
            } else {
                error!("Unsupported search option passed to go, use depth.");
                unimplemented!("Unsupported search option passed to go, use depth.");
            }
            use_stricter_cutoff = false;
        } else {
            error!("One of search or time is required to be passed to go.");
            panic!("One of search or time is required to be passed to go.");
        }

        let cutoff;
        match target_dur {
            Some(d) => {
                cutoff = Some(d.mul_f32(if use_stricter_cutoff { 0.4 } else { 0.5 }));
                self.cancel_search_at = Some(start_time.checked_add(d.mul_f32(1.1)).unwrap());
            }
            None => {
                cutoff = None;
                self.cancel_search_at = None;
            }
        }

        let mut depth = 1;
        let mut latest_result = None;
        loop {
            let result = self.alpha_beta_init(depth);
            if let Some(search_result) = result.search_result {
                let elapsed = start_time.elapsed();

                UciInterface::print_search_info(search_result.eval, &self.stats, &elapsed);

                if search_control != SearchControl::Infinite
                    && (result.end_search
                    || search_result.eval.abs() >= MATE_THRESHOLD
                    || depth >= max_depth
                    || match search_control {
                        SearchControl::Time => {
                            elapsed >= cutoff.unwrap()
                        }
                        SearchControl::Depth | SearchControl::Infinite => false,
                    })
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

    pub fn alpha_beta_init(&mut self, draft: u8) -> AlphaBetaResult {
        let mut alpha = -i16::MAX;
        let mut best_value = -i16::MAX;
        let mut best_move = None;
        self.rollback = MoveRollback::default();
        self.stats = SearchStats::default();
        let mut killers = [EMPTY_MOVE, EMPTY_MOVE];

        let mut moves;
        let tt_entry = self.transposition_table.get_entry(self.board.hash, TableType::Main, self.starting_halfmove);
        if let Some(tt_data) = tt_entry {

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

                // let bonus = (random::<u16>() % 11) as i16 - 5;
                let bonus = 0;
                assert!(bonus > -6);
                assert!(bonus < 6);
                let result = self.alpha_beta_recurse(-i16::MAX, -alpha, draft - 1, 1, &mut killers);
                let score;
                if let Some(e) = result {
                    score = -e;
                } else {
                    return AlphaBetaResult {
                        search_result: None,
                        end_search: true,
                    };
                }
                let eval = bonus + score;
                // debug!(
                //     "{}: score {score} bonus {bonus} result {result}",
                //     r#move.m.pretty_print(Some(self.board))
                // );

                self.board.unmake_move(&r#move.m, &mut self.rollback);

                if eval > best_value {
                    best_value = eval;
                    best_move = Some(r#move.m);
                    if eval > alpha {
                        alpha = eval;
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
    ) -> Option<i16> {
        if self.board.halfmove_clock >= 100
            || RepetitionTracker::test_threefold_repetition(self.board)
            || self.board.is_insufficient_material()
        {
            return Some(0);
        }

        self.board.white_to_move = !self.board.white_to_move;
        let in_check = self.board.can_capture_opponent_king(false);
        if in_check {
            draft += 1;
        }
        self.board.white_to_move = !self.board.white_to_move;

        if draft == 0 {
            self.stats.leaf_nodes += 1;
            self.stats.total_search_leaves += 1;

            if self.stats.total_search_leaves % 16384 == 16383 && self.cancel_search_at.is_some_and(|t| Instant::now() >= t) {
                return None;
            }

            return Some(self.quiescense_side_to_move_relative(alpha, beta, 255));
        }

        let mut best_value = -i16::MAX;
        let mut best_move = None;
        let mut new_killers = [EMPTY_MOVE, EMPTY_MOVE];

        let is_pv = alpha + 1 != beta;

        let mut moves;
        let tt_entry = self.transposition_table.get_entry(self.board.hash, TableType::Main, self.starting_halfmove);
        if let Some(tt_data) = tt_entry {
            if !is_pv && tt_data.draft >= draft {
                let eval = tt_data.get_eval(ply);

                match tt_data.get_move_type() {
                    transposition_table::MoveType::FailHigh => {
                        if eval >= beta {
                            self.update_killers_and_history(killers, &tt_data.important_move, ply);

                            return Some(eval);
                        }
                    }
                    transposition_table::MoveType::Best => {
                        return Some(eval);
                    }
                    transposition_table::MoveType::FailLow => {
                        if eval < alpha {
                            return Some(eval);
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

        // Null move pruning
        let our_side = if self.board.white_to_move { 0 } else { 1 };
        if beta < i16::MAX &&
                draft > 4 &&
                !in_check &&
                self.board.piece_bitboards[our_side][PIECE_PAWN as usize] | self.board.piece_bitboards[our_side][PIECE_KING as usize] != self.board.side_occupancy[our_side] {
            let mut null_move_killers = [EMPTY_MOVE, EMPTY_MOVE];
            self.board.make_null_move(&mut self.rollback);

            let result = self.alpha_beta_recurse(-beta, -(beta - 1), draft - 3, ply + 1, &mut null_move_killers);
            let eval;
            if let Some(e) = result {
                eval = -e;
            } else {
                return None;
            }

            self.board.unmake_null_move(&mut self.rollback);

            if eval >= beta {
                return Some(eval);
            }
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

                    if is_pv && reduction > 0 {
                        reduction -= 1;
                    }

                    if in_check && reduction > 0 {
                        reduction -= 1;
                    }

                    reduction = reduction.clamp(0, draft - 1)
                }

                let mut eval;
                if searched_moves == 0 {
                    let mut result = self.alpha_beta_recurse(-beta, -alpha, draft - reduction - 1, ply + 1, &mut new_killers);

                    if let Some(e) = result {
                        eval = -e;
                    } else {
                        return None;
                    }

                    if eval > alpha && reduction > 0 {
                        result = self.alpha_beta_recurse(-beta, -alpha, draft - 1, ply + 1, &mut new_killers);

                        if let Some(e) = result {
                            eval = -e;
                        } else {
                            return None;
                        }
                    }
                } else {
                    let mut result =
                        self.alpha_beta_recurse(-alpha - 1, -alpha, draft - reduction - 1, ply + 1, &mut new_killers);

                    if let Some(e) = result {
                        eval = -e;
                    } else {
                        return None;
                    }

                    // if result > alpha && reduction > 0 {
                    //     result = -self.alpha_beta_recurse(-alpha - 1, -alpha, draft - 1, ply + 1, &mut new_killers);
                    // }

                    if eval > alpha {
                        result = self.alpha_beta_recurse(-beta, -alpha, draft - 1, ply + 1, &mut new_killers);

                        if let Some(e) = result {
                            eval = -e;
                        } else {
                            return None;
                        }
                    }
                }

                self.board.unmake_move(&r#move.m, &mut self.rollback);
                searched_moves += 1;

                if eval >= beta {
                    self.update_killers_and_history(killers, &r#move.m, ply);

                    let penalty = -(ply as i16) * (ply as i16);
                    for m in searched_quiet_moves {
                        self.update_history(&m, penalty);
                    }

                    self.transposition_table.store_entry(
                        TTEntry::new(self.board.hash, r#move.m, MoveType::FailHigh, eval, draft, ply, self.starting_halfmove),
                        TableType::Main,
                    );

                    return Some(eval);
                }

                if eval > best_value {
                    best_value = eval;
                    best_move = Some(r#move.m);
                    if eval > alpha {
                        alpha = eval;
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
                return Some(self.board.evaluate_checkmate_side_to_move_relative(ply));
            } else {
                return Some(0);
            }
        }

        let entry_type = if alpha == best_value {
            MoveType::Best
        } else {
            MoveType::FailLow
        };
        self.transposition_table.store_entry(
            TTEntry::new(self.board.hash, best_move.unwrap(), entry_type, best_value, draft, ply, self.starting_halfmove),
            TableType::Main,
        );

        Some(best_value)
    }

    pub fn quiescense_side_to_move_relative(&mut self, mut alpha: i16, beta: i16, draft: u8) -> i16 {
        self.stats.quiescense_nodes += 1;

        if self.board.is_insufficient_material() {
            return 0;
        }

        let mut moves;
        let tt_entry = self
            .transposition_table
            .get_entry(self.board.hash, TableType::Quiescense, self.starting_halfmove);
        if let Some(tt_data) = tt_entry {
            let tt_eval = tt_data.get_eval(0);

            match tt_data.get_move_type() {
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
                let result = -self.quiescense_side_to_move_relative(-beta, -alpha, draft - 1);

                self.board.unmake_move(&r#move.m, &mut self.rollback);

                if result >= beta {
                    self.transposition_table.store_entry(
                        TTEntry::new(self.board.hash, r#move.m, MoveType::FailHigh, result, draft, 0, self.starting_halfmove),
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
                TTEntry::new(self.board.hash, bm, entry_type, best_value, 0, 0, self.starting_halfmove),
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
