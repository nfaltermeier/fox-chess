use std::{
    sync::mpsc::Receiver,
    time::{Duration, Instant},
};

use log::{debug, error};
use tinyvec::{TinyVec, tiny_vec};
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_KING, PIECE_MASK, PIECE_PAWN, PIECE_QUEEN},
    evaluate::{CENTIPAWN_VALUES, ENDGAME_GAME_STAGE_FOR_QUIESCENSE, MATE_THRESHOLD},
    move_generator::{
        MOVE_ARRAY_SIZE, MOVE_SCORE_CONST_HISTORY_MAX, MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_KILLER_1,
        MOVE_SCORE_KILLER_2, ScoredMove,
    },
    moves::{
        MOVE_FLAG_CAPTURE, MOVE_FLAG_CAPTURE_FULL, MOVE_FLAG_PROMOTION, MOVE_FLAG_PROMOTION_FULL, Move, MoveRollback,
    },
    repetition_tracker::RepetitionTracker,
    transposition_table::{self, MoveType, TTEntry, TableType, TranspositionTable},
    uci::UciInterface,
};

pub type HistoryTable = [[[i16; 64]; 6]; 2];
pub static DEFAULT_HISTORY_TABLE: HistoryTable = [[[0; 64]; 6]; 2];

pub type ContinuationHistoryTable = [[[[[i16; 64]; 6]; 64]; 6]; 2];

/// Each value is in addition to the last
static ASPIRATION_WINDOW_OFFSETS: [i16; 4] = [50, 150, 600, i16::MAX];

const EMPTY_MOVE: Move = Move { data: 0 };

#[derive(Default)]
pub struct SearchStats {
    pub depth: u8,
    pub total_nodes: u64,
    pub total_search_leaves: u64,
    pub aspiration_researches: u8,
}

#[derive(PartialEq, Eq)]
enum SearchControl {
    Unknown,
    Time,
    Depth,
    Infinite,
    Nodes,
}

#[derive(Clone)]
pub struct SearchResult {
    pub best_move: Move,
    pub score: i16,
}

pub struct AlphaBetaResult {
    pub search_result: Option<SearchResult>,
    pub end_search: bool,
    pub pv: TinyVec<[Move; 32]>,
}

pub struct Searcher<'a> {
    board: &'a mut Board,
    rollback: MoveRollback,
    pub stats: SearchStats,
    transposition_table: &'a mut TranspositionTable,
    history_table: &'a mut HistoryTable,
    starting_halfmove: u8,
    cancel_search_at: Option<Instant>,
    end_search: bool,
    starting_in_check: bool,
    stop_rx: &'a Receiver<()>,
    stop_received: bool,
    max_nodes: u64,
    continuation_history: &'a mut ContinuationHistoryTable,
    move_history: Vec<Move>,
}

impl<'a> Searcher<'a> {
    pub fn new(
        board: &'a mut Board,
        transposition_table: &'a mut TranspositionTable,
        history_table: &'a mut HistoryTable,
        stop_rx: &'a Receiver<()>,
        continuation_history: &'a mut ContinuationHistoryTable,
    ) -> Self {
        let starting_halfmove = board.halfmove_clock;

        let starting_in_check = board.is_in_check(false);

        Self {
            board,
            rollback: MoveRollback::default(),
            stats: SearchStats::default(),
            transposition_table,
            history_table,
            starting_halfmove,
            cancel_search_at: None,
            end_search: false,
            starting_in_check,
            stop_rx,
            stop_received: false,
            max_nodes: u64::MAX,
            continuation_history,
            move_history: vec![],
        }
    }

    pub fn iterative_deepening_search(
        &mut self,
        time: &Option<UciTimeControl>,
        search: &Option<UciSearchControl>,
    ) -> SearchResult {
        let start_time = Instant::now();
        let mut target_dur = None;
        let mut search_control: SearchControl;
        let mut max_depth = 40;
        let use_stricter_time_cutoff;

        if let Some(t) = time {
            match t {
                UciTimeControl::TimeLeft {
                    white_time,
                    black_time,
                    white_increment,
                    black_increment,
                    // For TC where you get more time after playing a certain number of moves. May need to consider this if going for tournaments.
                    moves_to_go: _,
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
                        25
                    } else if self.board.fullmove_counter < 25 {
                        20
                    } else {
                        30
                    };

                    let time_left = time_left.as_ref().unwrap().to_std().unwrap();
                    target_dur = Some(time_left.checked_div(divisor).unwrap());

                    if let Some(inc) = increment {
                        let inc = inc.to_std().unwrap();

                        if time_left > inc.saturating_mul(2) {
                            target_dur = Some(target_dur.unwrap().saturating_add(inc.mul_f32(0.7)));

                            use_stricter_time_cutoff = time_left < Duration::from_secs(1);
                        } else {
                            use_stricter_time_cutoff = true;
                        }
                    } else {
                        use_stricter_time_cutoff = time_left < Duration::from_secs(5);
                    }

                    search_control = SearchControl::Time;
                }
                UciTimeControl::MoveTime(time_delta) => {
                    target_dur = Some(time_delta.to_std().unwrap());
                    search_control = SearchControl::Time;
                    use_stricter_time_cutoff = true;
                }
                UciTimeControl::Ponder => {
                    unimplemented!("uci go ponder");
                }
                UciTimeControl::Infinite => {
                    search_control = SearchControl::Infinite;
                    use_stricter_time_cutoff = false;
                }
            }
        } else {
            search_control = SearchControl::Unknown;
            use_stricter_time_cutoff = false;
        }

        if let Some(s) = search {
            if s.depth.is_some() {
                if search_control == SearchControl::Unknown {
                    search_control = SearchControl::Depth;
                }
                max_depth = s.depth.unwrap();
            } else if s.nodes.is_some() {
                if search_control == SearchControl::Unknown {
                    search_control = SearchControl::Nodes;
                }
                self.max_nodes = s.nodes.unwrap();
            }
        }

        if search_control == SearchControl::Unknown {
            error!("Please specify wtime and btime, infinite, depth, or nodes when calling go.");
            panic!("Please specify wtime and btime, infinite, depth, or nodes when calling go.");
        }

        let cutoff;
        match target_dur {
            Some(d) => {
                cutoff = Some(d.mul_f32(if use_stricter_time_cutoff { 0.3 } else { 0.5 }));
                self.cancel_search_at = Some(
                    start_time
                        .checked_add(d.mul_f32(if use_stricter_time_cutoff { 1.1 } else { 2.0 }))
                        .unwrap(),
                );
            }
            None => {
                cutoff = None;
                self.cancel_search_at = None;
            }
        }

        let mut depth = 1;
        let mut latest_result = None;
        loop {
            let result = self.alpha_beta_init(depth, latest_result.clone());
            if let Some(search_result) = result.search_result {
                // Max nodes are infrequently checked in the search so do an additional check here
                if self.stats.total_nodes >= self.max_nodes {
                    return latest_result
                        .expect("iterative_deepening_search exceeded max nodes before completing any searches");
                }

                let elapsed = start_time.elapsed();

                UciInterface::print_search_info(search_result.score, &self.stats, &elapsed, &result.pv);

                self.stats.aspiration_researches = 0;

                if self.stop_received
                    || (search_control != SearchControl::Infinite
                        && (result.end_search
                            || search_result.score.abs() >= MATE_THRESHOLD
                            || depth >= max_depth
                            || (matches!(search_control, SearchControl::Time) && elapsed >= cutoff.unwrap())))
                {
                    return search_result;
                }

                latest_result = Some(search_result);
            } else {
                if !self.stop_received {
                    debug!("Cancelled search of depth {depth} due to exceeding time budget or max nodes being reached");
                }

                return latest_result
                    .expect("iterative_deepening_search exceeded cancel_search_time or max nodes before completing any searches");
            }

            depth += 1;
        }
    }

    pub fn alpha_beta_init(&mut self, draft: u8, last_result: Option<SearchResult>) -> AlphaBetaResult {
        self.rollback = MoveRollback::default();
        self.stats.depth = draft;
        self.end_search = false;

        let mut killers = [EMPTY_MOVE, EMPTY_MOVE];

        let mut alpha;
        let mut beta;
        let mut alpha_window_index;
        let mut beta_window_index;
        if draft < 5 {
            alpha = -i16::MAX;
            beta = i16::MAX;
            alpha_window_index = ASPIRATION_WINDOW_OFFSETS.len();
            beta_window_index = ASPIRATION_WINDOW_OFFSETS.len();
        } else {
            let last_score = last_result.unwrap().score;
            alpha = last_score - ASPIRATION_WINDOW_OFFSETS[0];
            beta = last_score + ASPIRATION_WINDOW_OFFSETS[0];
            alpha_window_index = 1;
            beta_window_index = 1;
        }

        let mut pv = tiny_vec!();

        let score = loop {
            let result = self.alpha_beta_recurse(
                alpha,
                beta,
                draft,
                0,
                &mut killers,
                self.starting_in_check,
                true,
                &mut pv,
            );

            let score;
            if let Ok(e) = result {
                score = e;
            } else {
                self.move_history.clear();

                // In this case the state of the board will not been reset back to the starting state
                return AlphaBetaResult {
                    search_result: None,
                    end_search: true,
                    pv,
                };
            }

            debug_assert!(self.rollback.is_empty());
            debug_assert!(self.move_history.is_empty());

            if score <= alpha {
                self.stats.aspiration_researches += 1;

                while score <= alpha {
                    alpha = alpha
                        .saturating_sub(ASPIRATION_WINDOW_OFFSETS[alpha_window_index])
                        .min(-i16::MAX);
                    alpha_window_index += 1;
                }
            } else if score >= beta {
                self.stats.aspiration_researches += 1;

                while score >= beta {
                    beta = beta.saturating_add(ASPIRATION_WINDOW_OFFSETS[beta_window_index]);
                    beta_window_index += 1;
                }
            } else {
                break score;
            }
        };

        if pv.is_empty() {
            error!("PV is empty in alpha_beta_init");
            panic!("PV is empty in alpha_beta_init")
        }

        let best_move = pv.last().unwrap();

        AlphaBetaResult {
            search_result: Some(SearchResult {
                best_move: *best_move,
                score,
            }),
            end_search: self.end_search,
            pv,
        }
    }

    fn alpha_beta_recurse(
        &mut self,
        mut alpha: i16,
        beta: i16,
        mut draft: u8,
        ply: u8,
        killers: &mut [Move; 2],
        in_check: bool,
        can_null_move: bool,
        parent_pv: &mut TinyVec<[Move; 32]>,
    ) -> Result<i16, ()> {
        self.stats.total_nodes += 1;

        if ply != 0
            && (self.board.halfmove_clock >= 100
                || RepetitionTracker::test_repetition(self.board)
                || self.board.is_insufficient_material())
        {
            parent_pv.clear();
            return Ok(0);
        }

        if in_check {
            draft += 1;
        }

        if draft == 0 {
            self.stats.total_search_leaves += 1;

            if self.stats.total_search_leaves % 16384 == 16383 {
                let stop_received = matches!(self.stop_rx.try_recv(), Ok(()));
                if stop_received
                    || self.cancel_search_at.is_some_and(|t| Instant::now() >= t)
                    || self.stats.total_nodes >= self.max_nodes
                {
                    if stop_received {
                        self.stop_received = stop_received;
                    }

                    return Err(());
                }
            }

            parent_pv.clear();

            return Ok(self.quiescense_side_to_move_relative(alpha, beta, 255));
        }

        let mut best_score = -i16::MAX;
        let mut best_move = None;
        let mut new_killers = [EMPTY_MOVE, EMPTY_MOVE];

        let is_pv = alpha + 1 != beta;

        let mut moves: TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]>;
        let tt_entry = self
            .transposition_table
            .get_entry(self.board.hash, TableType::Main, self.starting_halfmove);
        if let Some(tt_data) = tt_entry {
            if !is_pv && tt_data.draft >= draft {
                let tt_score = tt_data.get_score(ply);

                match tt_data.move_type {
                    transposition_table::MoveType::FailHigh => {
                        if tt_score >= beta {
                            // should history be updated here?
                            let mut relevant_cont_hist = get_relevant_cont_hist(
                                &self.move_history,
                                &self.board,
                                &mut *self.continuation_history,
                            );
                            update_killers_and_history(
                                &self.board,
                                &mut self.history_table,
                                killers,
                                &tt_data.important_move,
                                ply,
                                &mut relevant_cont_hist,
                            );

                            return Ok(tt_score);
                        }
                    }
                    transposition_table::MoveType::Best => {
                        return Ok(tt_score);
                    }
                    transposition_table::MoveType::FailLow => {
                        if tt_score < alpha {
                            return Ok(tt_score);
                        }
                    }
                }
            }

            moves = tiny_vec!(ScoredMove {
                m: tt_data.important_move,
                score: 1,
            });
        } else {
            moves = tiny_vec!();
        }

        let mut pv: TinyVec<[Move; 32]> = tiny_vec!();

        let mut futility_prune = false;
        if draft < 4 && !is_pv && !in_check && alpha.abs() < 2000 && beta.abs() < 2000 {
            let eval = self.board.evaluate_side_to_move_relative();

            // Reverse futility pruning
            if eval - 150 * (draft as i16) >= beta {
                return Ok((eval + beta) / 2);
            }

            futility_prune = (eval + 300 + 200 * (draft - 1) as i16) < alpha;

            // Razoring
            if (eval + 300 + 300 * (draft - 1) as i16) < alpha {
                let score = self.quiescense_side_to_move_relative(alpha, beta, 255);
                if score < alpha {
                    return Ok(score);
                }
            }
        }

        // Null move pruning
        let our_side = if self.board.white_to_move { 0 } else { 1 };
        if can_null_move
            && !is_pv
            && beta < i16::MAX
            && draft > 4
            && !in_check
            && self.board.piece_bitboards[our_side][PIECE_PAWN as usize]
                | self.board.piece_bitboards[our_side][PIECE_KING as usize]
                != self.board.side_occupancy[our_side]
        {
            let mut null_move_killers = [EMPTY_MOVE, EMPTY_MOVE];
            self.board.make_null_move(&mut self.rollback);
            self.move_history.push(EMPTY_MOVE);

            // Need to ensure draft >= 1
            let reduction = 3 + draft / 6;

            let nmp_score = -self.alpha_beta_recurse(
                -beta,
                -(beta - 1),
                draft - reduction,
                ply + 1,
                &mut null_move_killers,
                false,
                false,
                &mut pv,
            )?;

            self.board.unmake_null_move(&mut self.rollback);
            self.move_history.pop();

            if nmp_score >= beta {
                return Ok(nmp_score);
            }
        }

        // Internal Iterative Deepening
        if moves.is_empty() && is_pv && draft > 5 {
            let mut iid_pv = tiny_vec!();
            self.alpha_beta_recurse(alpha, beta, draft - 2, ply, killers, in_check, can_null_move, &mut iid_pv)?;

            let tt_entry = self
                .transposition_table
                .get_entry(self.board.hash, TableType::Main, self.starting_halfmove);
            if let Some(tt_data) = tt_entry {
                moves.push(ScoredMove {
                    m: tt_data.important_move,
                    score: 1,
                });
            }
        }

        let mut searched_quiet_moves: TinyVec<[Move; 64]> = tiny_vec!();
        let mut searched_moves = 0;
        let mut has_legal_move = false;
        let mut improved_alpha = false;

        // Round 0 is the tt move, round 1 is regular move gen
        for round in 0..2 {
            for move_index in 0..moves.len() {
                {
                    // Perform one iteration of selection sort every time another move needs to be evaluated
                    let mut best_move_score = moves[move_index].score;
                    let mut best_move_index = move_index;

                    for sort_index in (move_index + 1)..moves.len() {
                        if moves[sort_index].score > best_move_score {
                            best_move_score = moves[sort_index].score;
                            best_move_index = sort_index;
                        }
                    }

                    moves.swap(move_index, best_move_index);
                }

                let r#move = &moves[move_index];

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

                has_legal_move = true;
                let gives_check = self.board.is_in_check(false);

                // Futility pruning and late move pruning
                if (futility_prune
                    || (!is_pv && !in_check && searched_moves >= 6 && r#move.score < -750 - 50 * draft as i16))
                    && searched_moves >= 1
                    && !gives_check
                    && r#move.m.data & (MOVE_FLAG_CAPTURE_FULL | MOVE_FLAG_PROMOTION_FULL) == 0
                {
                    self.board.unmake_move(&r#move.m, &mut self.rollback);
                    continue;
                }

                self.move_history.push(r#move.m);

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

                let mut score;
                if searched_moves == 0 || ply == 0 {
                    // Use reduction
                    score = -self.alpha_beta_recurse(
                        -beta,
                        -alpha,
                        draft - reduction - 1,
                        ply + 1,
                        &mut new_killers,
                        gives_check,
                        can_null_move,
                        &mut pv,
                    )?;

                    if score > alpha && reduction > 0 {
                        // Do a full search
                        score = -self.alpha_beta_recurse(
                            -beta,
                            -alpha,
                            draft - 1,
                            ply + 1,
                            &mut new_killers,
                            gives_check,
                            can_null_move,
                            &mut pv,
                        )?;
                    }
                } else {
                    // Use null window and reduction
                    score = -self.alpha_beta_recurse(
                        -alpha - 1,
                        -alpha,
                        draft - reduction - 1,
                        ply + 1,
                        &mut new_killers,
                        gives_check,
                        can_null_move,
                        &mut pv,
                    )?;

                    if score > alpha {
                        // Do a full search
                        score = -self.alpha_beta_recurse(
                            -beta,
                            -alpha,
                            draft - 1,
                            ply + 1,
                            &mut new_killers,
                            gives_check,
                            can_null_move,
                            &mut pv,
                        )?;
                    }
                }

                self.board.unmake_move(&r#move.m, &mut self.rollback);
                self.move_history.pop();
                searched_moves += 1;

                if score >= beta {
                    let mut relevant_cont_hist =
                        get_relevant_cont_hist(&self.move_history, &self.board, &mut *self.continuation_history);

                    update_killers_and_history(
                        &self.board,
                        &mut self.history_table,
                        killers,
                        &r#move.m,
                        ply,
                        &mut relevant_cont_hist,
                    );

                    let penalty = -(ply as i16) * (ply as i16);
                    for m in searched_quiet_moves {
                        update_history(
                            &self.board,
                            &mut self.history_table,
                            &m,
                            penalty,
                            &mut relevant_cont_hist,
                        );
                    }

                    self.transposition_table.store_entry(
                        TTEntry::new(
                            self.board.hash,
                            r#move.m,
                            MoveType::FailHigh,
                            score,
                            draft,
                            ply,
                            self.starting_halfmove,
                        ),
                        TableType::Main,
                    );

                    return Ok(score);
                }

                if score > best_score {
                    best_score = score;
                    best_move = Some(r#move.m);
                    if score > alpha {
                        alpha = score;
                        improved_alpha = true;

                        if is_pv {
                            *parent_pv = pv.clone();
                            parent_pv.push(r#move.m);
                        }
                    }
                }

                // TODO: change to check for capture flag
                if r#move.m.flags() == 0 {
                    searched_quiet_moves.push(r#move.m);
                }
            }

            if round == 0 {
                if !in_check {
                    moves = self.board.generate_pseudo_legal_moves_with_history(self.history_table);
                } else {
                    moves = self.board.generate_pseudo_legal_check_evasions(self.history_table);
                }

                self.apply_history_to_move_scores(&mut moves);

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
            }
        }

        // Assuming no bug with move generation...
        if !has_legal_move {
            if ply == 0 {
                error!(
                    "Tried to search on a position but found no moves. Position: {:#?}",
                    self.board
                );
                panic!("Found no legal moves from the root of the search")
            } else if in_check {
                parent_pv.clear();
                return Ok(self.board.evaluate_checkmate_side_to_move_relative(ply));
            } else {
                parent_pv.clear();
                return Ok(0);
            }
        } else if searched_moves == 1 && ply == 0 {
            self.end_search = true;
        }

        let entry_type = if improved_alpha {
            MoveType::Best
        } else {
            MoveType::FailLow
        };
        self.transposition_table.store_entry(
            TTEntry::new(
                self.board.hash,
                best_move.unwrap(),
                entry_type,
                best_score,
                draft,
                ply,
                self.starting_halfmove,
            ),
            TableType::Main,
        );

        Ok(best_score)
    }

    pub fn quiescense_side_to_move_relative(&mut self, mut alpha: i16, beta: i16, draft: u8) -> i16 {
        self.stats.total_nodes += 1;

        if self.board.is_insufficient_material() {
            return 0;
        }

        let mut moves: TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]>;
        let tt_entry =
            self.transposition_table
                .get_entry(self.board.hash, TableType::Quiescense, self.starting_halfmove);
        if let Some(tt_data) = tt_entry {
            let tt_eval = tt_data.get_score(0);

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

            moves = tiny_vec!(ScoredMove {
                m: tt_data.important_move,
                score: 1,
            });
        } else {
            moves = tiny_vec!();
        }

        let stand_pat = self.board.evaluate_side_to_move_relative();

        if stand_pat >= beta {
            return stand_pat;
        }

        if self.board.game_stage > ENDGAME_GAME_STAGE_FOR_QUIESCENSE
            && stand_pat + CENTIPAWN_VALUES[PIECE_QUEEN as usize] + 100 < alpha
        {
            return stand_pat;
        }

        if alpha < stand_pat {
            alpha = stand_pat;
        }

        let mut best_score = stand_pat;
        let mut best_move = None;
        let mut improved_alpha = false;

        // Round 0 is the tt move, round 1 is regular move gen
        for round in 0..2 {
            for move_index in 0..moves.len() {
                {
                    // Perform one iteration of selection sort every time another move needs to be evaluated
                    let mut best_move_score = moves[move_index].score;
                    let mut best_move_index = move_index;

                    for sort_index in (move_index + 1)..moves.len() {
                        if moves[sort_index].score > best_move_score {
                            best_move_score = moves[sort_index].score;
                            best_move_index = sort_index;
                        }
                    }

                    moves.swap(move_index, best_move_index);
                }
                let r#move = &moves[move_index];

                if round == 1 && tt_entry.is_some_and(|v| v.important_move == r#move.m) {
                    continue;
                }

                // SEE is coded to be run before the move is made so have to do it before testing legality
                // Typical implementations also only check if the score is better than a threshold instead of calculating the whole thing.
                if self.board.static_exchange_eval(r#move.m) < 0 {
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
                let score = -self.quiescense_side_to_move_relative(-beta, -alpha, draft - 1);

                self.board.unmake_move(&r#move.m, &mut self.rollback);

                if score >= beta {
                    self.transposition_table.store_entry(
                        TTEntry::new(
                            self.board.hash,
                            r#move.m,
                            MoveType::FailHigh,
                            score,
                            draft,
                            0,
                            self.starting_halfmove,
                        ),
                        TableType::Quiescense,
                    );

                    return score;
                }

                if best_score < score {
                    best_score = score;
                    best_move = Some(r#move.m);

                    if alpha < score {
                        alpha = score;
                        improved_alpha = true;
                    }
                }
            }

            if round == 0 {
                moves = self.board.generate_pseudo_legal_capture_moves();
            }
        }

        if let Some(bm) = best_move {
            let entry_type = if improved_alpha {
                MoveType::Best
            } else {
                MoveType::FailLow
            };
            self.transposition_table.store_entry(
                TTEntry::new(
                    self.board.hash,
                    bm,
                    entry_type,
                    best_score,
                    0,
                    0,
                    self.starting_halfmove,
                ),
                TableType::Quiescense,
            );
        }

        best_score
    }

    fn apply_history_to_move_scores(&mut self, moves: &mut TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]>) {
        if let Some(relevant_cont_hist) =
            get_relevant_cont_hist(&self.move_history, &self.board, &mut *self.continuation_history)
        {
            for m in moves {
                if m.m.data & MOVE_FLAG_CAPTURE_FULL != 0 {
                    continue;
                }

                let piece_to_move = self.board.get_piece_64(m.m.from() as usize);
                m.score += relevant_cont_hist[((piece_to_move & PIECE_MASK) - 1) as usize][m.m.to() as usize];
            }
        }
    }
}

#[inline]
fn update_killers_and_history(
    board: &Board,
    history_table: &mut HistoryTable,
    killers: &mut [Move; 2],
    m: &Move,
    ply_depth: u8,
    relevant_cont_hist: &mut Option<&mut [[i16; 64]; 6]>,
) {
    // TODO: Change this to check for capture flag specifically
    if m.flags() != 0 {
        return;
    }

    update_history(
        board,
        history_table,
        m,
        (ply_depth as i16) * (ply_depth as i16),
        relevant_cont_hist,
    );

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
fn update_history(
    board: &Board,
    history_table: &mut HistoryTable,
    m: &Move,
    bonus: i16,
    relevant_cont_hist: &mut Option<&mut [[i16; 64]; 6]>,
) {
    // from https://www.chessprogramming.org/History_Heuristic
    let piece_type = (board.get_piece_64(m.from() as usize) & PIECE_MASK) as usize;
    let history_color_value = if board.white_to_move { 0 } else { 1 };
    let to = m.to() as usize;

    let clamped_history_bonus = (bonus as i32).clamp(-MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_HISTORY_MAX);
    let current_history = &mut history_table[history_color_value][piece_type - 1][to];
    *current_history += (clamped_history_bonus
        - ((*current_history as i32) * clamped_history_bonus.abs() / MOVE_SCORE_HISTORY_MAX))
        as i16;

    let clamped_const_history_bonus = (bonus as i32).clamp(-MOVE_SCORE_CONST_HISTORY_MAX, MOVE_SCORE_CONST_HISTORY_MAX);
    if let Some(relevant_cont_hist) = relevant_cont_hist {
        let current_cont_hist = &mut relevant_cont_hist[piece_type - 1][to];
        *current_cont_hist += (clamped_const_history_bonus
            - ((*current_cont_hist as i32) * clamped_const_history_bonus.abs() / MOVE_SCORE_CONST_HISTORY_MAX))
            as i16;
    }
}

#[inline]
fn get_relevant_cont_hist<'a>(
    move_history: &Vec<Move>,
    board: &Board,
    continuation_history: &'a mut ContinuationHistoryTable,
) -> Option<&'a mut [[i16; 64]; 6]> {
    if let Some(last_move) = move_history.last() {
        if *last_move != EMPTY_MOVE {
            let last_moved_to = last_move.to() as usize;
            let last_moved_piece = board.get_piece_64(last_moved_to);
            return Some(
                &mut continuation_history[if board.white_to_move { 0 } else { 1 }]
                    [((last_moved_piece & PIECE_MASK) - 1) as usize][last_moved_to],
            );
        }
    }

    None
}
