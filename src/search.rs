use std::{
    cmp::Reverse,
    collections::HashSet,
    sync::mpsc::Receiver,
    time::{Duration, Instant},
    u64,
};

use log::{debug, error};
use tinyvec::{ArrayVec, TinyVec, array_vec, tiny_vec};
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, HASH_VALUES, PIECE_KING, PIECE_MASK, PIECE_PAWN, PIECE_QUEEN},
    evaluate::{CENTIPAWN_VALUES, ENDGAME_GAME_STAGE_FOR_QUIESCENSE, MATE_THRESHOLD},
    move_generator::{
        ENABLE_UNMAKE_MOVE_TEST, MOVE_ARRAY_SIZE, MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_KILLER_1, MOVE_SCORE_KILLER_2,
        ScoredMove,
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

/// Each value is in addition to the last
static ASPIRATION_WINDOW_OFFSETS: [i16; 4] = [50, 150, 600, i16::MAX];

const EMPTY_MOVE: Move = Move { data: 0 };

// pub const TEST_TT_FOR_HASH_COLLISION: bool = true;

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
    pub eval: i16,
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
}

impl<'a> Searcher<'a> {
    pub fn new(
        board: &'a mut Board,
        transposition_table: &'a mut TranspositionTable,
        history_table: &'a mut HistoryTable,
        stop_rx: &'a Receiver<()>,
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

                UciInterface::print_search_info(search_result.eval, &self.stats, &elapsed, &result.pv);

                self.stats.aspiration_researches = 0;

                if self.stop_received
                    || (search_control != SearchControl::Infinite
                        && (result.end_search
                            || search_result.eval.abs() >= MATE_THRESHOLD
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
            let last_score = last_result.unwrap().eval;
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
                // In this case the state of the board will not been reset back to the starting state
                return AlphaBetaResult {
                    search_result: None,
                    end_search: true,
                    pv,
                };
            }

            debug_assert!(self.rollback.is_empty());

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
                best_move: best_move.clone(),
                eval: score,
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

        if self.board.halfmove_clock >= 100
            || RepetitionTracker::test_threefold_repetition(self.board)
            || self.board.is_insufficient_material()
        {
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
                let eval = tt_data.get_score(ply);

                match tt_data.get_move_type() {
                    transposition_table::MoveType::FailHigh => {
                        if eval >= beta {
                            self.update_killers_and_history(killers, &tt_data.important_move, ply);

                            return Ok(eval);
                        }
                    }
                    transposition_table::MoveType::Best => {
                        return Ok(eval);
                    }
                    transposition_table::MoveType::FailLow => {
                        if eval < alpha {
                            return Ok(eval);
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

            // Need to ensure draft >= 1
            let reduction = 3 + draft / 6;

            let eval = -self.alpha_beta_recurse(
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

            if eval >= beta {
                return Ok(eval);
            }
        }

        let futility_prune = draft < 4
            && !is_pv
            && !in_check
            && alpha.abs() < 2000
            && beta.abs() < 2000
            && (self.board.evaluate_side_to_move_relative() + 300 + 200 * (draft - 1) as i16) < alpha;

        let mut searched_quiet_moves = Vec::new();
        let mut searched_moves = 0;
        let mut has_legal_move = false;

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
                if futility_prune
                    && searched_moves >= 1
                    && !gives_check
                    && r#move.m.data & (MOVE_FLAG_CAPTURE_FULL | MOVE_FLAG_PROMOTION_FULL) == 0
                {
                    self.board.unmake_move(&r#move.m, &mut self.rollback);
                    continue;
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
                searched_moves += 1;

                if score >= beta {
                    self.update_killers_and_history(killers, &r#move.m, ply);

                    let penalty = -(ply as i16) * (ply as i16);
                    for m in searched_quiet_moves {
                        self.update_history(&m, penalty);
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

                        if is_pv {
                            *parent_pv = pv.clone();
                            parent_pv.push(r#move.m);
                        }
                    }
                }

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
                return Ok(self.board.evaluate_checkmate_side_to_move_relative(ply));
            } else {
                return Ok(0);
            }
        } else if searched_moves == 1 && ply == 0 {
            self.end_search = true;
        }

        let entry_type = if alpha == best_score {
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
                    }
                }
            }

            if round == 0 {
                moves = self.board.generate_pseudo_legal_capture_moves();
            }
        }

        if let Some(bm) = best_move {
            let entry_type = if alpha == best_score {
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
