use std::{
    collections::VecDeque,
    sync::mpsc::Receiver,
    time::Instant,
};

use arrayvec::ArrayVec;
use log::{debug, error};
use tinyvec::{TinyVec, tiny_vec};
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_KING, PIECE_MASK, PIECE_PAWN, PIECE_QUEEN},
    evaluate::{ENDGAME_GAME_STAGE_FOR_QUIESCENSE, MATE_THRESHOLD},
    move_generator::{MOVE_ARRAY_SIZE, MOVE_SCORE_CONST_HISTORY_MAX, MOVE_SCORE_HISTORY_MAX, ScoredMove},
    move_generator_struct::{GetMoveResult, MoveGenerator},
    moves::{
        MOVE_FLAG_CAPTURE, MOVE_FLAG_CAPTURE_FULL, MOVE_FLAG_PROMOTION, MOVE_FLAG_PROMOTION_FULL, Move, MoveRollback,
    },
    repetition_tracker::RepetitionTracker,
    texel::{DEFAULT_PARAMS, EvalParams, FeatureIndex},
    time_management::{get_cutoff_times, modify_cutoff_time},
    transposition_table::{self, MoveType, TTEntry, TranspositionTable},
    uci::UciInterface,
    uci_required_options_helper::RequiredUciOptions,
};
use crate::eval_values::CENTIPAWN_VALUES_MIDGAME;

pub type HistoryTable = [[[i16; 64]; 6]; 2];
pub static DEFAULT_HISTORY_TABLE: HistoryTable = [[[0; 64]; 6]; 2];

pub type ContinuationHistoryTable = [[[[[i16; 64]; 6]; 64]; 6]; 2];

/// Each value is in addition to the last
static ASPIRATION_WINDOW_OFFSETS: [i16; 4] = [50, 150, 600, i16::MAX];

pub const EMPTY_MOVE: Move = Move { data: 0 };

#[derive(Default)]
pub struct SearchStats {
    pub depth: u8,
    pub current_iteration_total_nodes: u64,
    pub previous_iterations_total_nodes: u64,
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

struct PvData {
    /// PVs are stored as a stack, FILO
    pub pv: TinyVec<[Move; 32]>,
    pub search_result: SearchResult,
}

pub struct Searcher<'a> {
    board: &'a mut Board,
    rollback: MoveRollback,
    pub stats: SearchStats,
    transposition_table: &'a mut TranspositionTable,
    history_table: &'a mut HistoryTable,
    starting_fullmove: u8,
    hard_cutoff_time: Option<Instant>,
    single_root_move: bool,
    starting_in_check: bool,
    stop_rx: &'a Receiver<()>,
    stop_received: bool,
    total_max_nodes: u64,
    max_nodes_for_current_iteration: u64,
    continuation_history: &'a mut ContinuationHistoryTable,
    move_history: Vec<Move>,
    multi_pv: u8,
    root_pvs: VecDeque<PvData>,
    extra_uci_options: RequiredUciOptions,
    root_pv_branch_nodes: u64,
    pv_nodes_fractions: VecDeque<f32>,
    contempt: i16,
    white_started_search: bool,
}

impl<'a> Searcher<'a> {
    pub fn new(
        board: &'a mut Board,
        transposition_table: &'a mut TranspositionTable,
        history_table: &'a mut HistoryTable,
        stop_rx: &'a Receiver<()>,
        continuation_history: &'a mut ContinuationHistoryTable,
        multi_pv: u8,
        extra_uci_options: RequiredUciOptions,
        contempt: i16,
    ) -> Self {
        let starting_fullmove = 0;

        let starting_in_check = board.is_in_check(false);

        let white_started_search = board.white_to_move;

        assert!(multi_pv >= 1);

        Self {
            board,
            rollback: MoveRollback::default(),
            stats: SearchStats::default(),
            transposition_table,
            history_table,
            starting_fullmove,
            hard_cutoff_time: None,
            single_root_move: false,
            starting_in_check,
            stop_rx,
            stop_received: false,
            total_max_nodes: u64::MAX,
            max_nodes_for_current_iteration: u64::MAX,
            continuation_history,
            move_history: vec![],
            multi_pv,
            root_pvs: VecDeque::with_capacity(multi_pv as usize),
            extra_uci_options,
            root_pv_branch_nodes: 0,
            pv_nodes_fractions: VecDeque::new(),
            contempt,
            white_started_search,
        }
    }

    pub fn iterative_deepening_search(
        &mut self,
        time: &Option<UciTimeControl>,
        search: &Option<UciSearchControl>,
    ) -> SearchResult {
        let start_time = Instant::now();
        let mut soft_cutoff_time = None;
        let mut search_control: SearchControl;
        let mut max_depth = 40;
        let mut cutoff_times = None;

        self.hard_cutoff_time = None;
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

                    cutoff_times = Some(get_cutoff_times(time_left, increment, &start_time, 42));
                    soft_cutoff_time = Some(cutoff_times.as_ref().unwrap().soft_cutoff);
                    self.hard_cutoff_time = Some(cutoff_times.as_ref().unwrap().hard_cutoff);

                    search_control = SearchControl::Time;
                }
                UciTimeControl::MoveTime(time_delta) => {
                    let target_dur = time_delta.to_std().unwrap();
                    soft_cutoff_time = Some(target_dur);
                    self.hard_cutoff_time = Some(start_time.checked_add(target_dur).unwrap());
                    search_control = SearchControl::Time;
                }
                UciTimeControl::Ponder => {
                    unimplemented!("uci go ponder");
                }
                UciTimeControl::Infinite => {
                    search_control = SearchControl::Infinite;
                }
            }
        } else {
            search_control = SearchControl::Unknown;
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
                self.total_max_nodes = s.nodes.unwrap();
            }
        }

        if search_control == SearchControl::Unknown {
            error!("Please specify wtime and btime, infinite, depth, or nodes when calling go.");
            panic!("Please specify wtime and btime, infinite, depth, or nodes when calling go.");
        }

        let mut depth = 1;
        let mut latest_result = None;
        loop {
            let end_search = self.alpha_beta_init(depth, latest_result.clone());
            if let Some(best_pv) = self.root_pvs.front()
                && let Some(worst_pv) = self.root_pvs.back()
            {
                // Max nodes are infrequently checked in the search so do an additional check here
                if self.stats.current_iteration_total_nodes >= self.max_nodes_for_current_iteration {
                    return latest_result
                        .expect("iterative_deepening_search exceeded max nodes before completing any searches");
                }

                let elapsed = start_time.elapsed();

                for (i, pv) in self.root_pvs.iter().enumerate() {
                    UciInterface::print_search_info(
                        pv.search_result.score,
                        &self.stats,
                        &elapsed,
                        &self.transposition_table,
                        &pv.pv,
                        self.starting_fullmove,
                        i as u8 + 1,
                    );
                }

                self.stats.aspiration_researches = 0;

                if self.stop_received
                    || (search_control != SearchControl::Infinite
                        && (end_search
                            || worst_pv.search_result.score.abs() >= MATE_THRESHOLD
                            || depth >= max_depth))
                {
                    return best_pv.search_result.clone();
                }

                if matches!(search_control, SearchControl::Time) {
                    if let Some(cutoff_times) = &cutoff_times {
                        let pv_nodes_fraction = self.root_pv_branch_nodes as f32 / self.stats.current_iteration_total_nodes as f32;

                        if self.pv_nodes_fractions.len() >= 3 {
                            self.pv_nodes_fractions.pop_back();
                        }

                        self.pv_nodes_fractions.push_front(pv_nodes_fraction);

                        soft_cutoff_time = Some(modify_cutoff_time(cutoff_times, &self.pv_nodes_fractions));
                    }

                    if elapsed >= soft_cutoff_time.unwrap() {
                        return best_pv.search_result.clone();
                    }
                }

                latest_result = Some(best_pv.search_result.clone());
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

    pub fn alpha_beta_init(&mut self, draft: u8, last_result: Option<SearchResult>) -> bool {
        self.rollback = MoveRollback::default();
        self.stats.depth = draft;
        self.single_root_move = false;

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

        loop {
            self.root_pvs.clear();
            self.stats.previous_iterations_total_nodes += self.stats.current_iteration_total_nodes;
            self.stats.current_iteration_total_nodes = 0;
            self.max_nodes_for_current_iteration = self.total_max_nodes - self.stats.previous_iterations_total_nodes;

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

            if result.is_err() {
                self.move_history.clear();
                self.root_pvs.clear();

                // In this case the state of the board will not been reset back to the starting state
                return true;
            }

            debug_assert!(self.rollback.is_empty());
            debug_assert!(self.move_history.is_empty());

            let low_score = self
                .root_pvs
                .back()
                .expect("No root pv found in alpha_beta_init after successful search")
                .search_result
                .score;
            let high_score = self
                .root_pvs
                .front()
                .expect("No root pv found in alpha_beta_init after successful search")
                .search_result
                .score;

            if low_score <= alpha {
                self.stats.aspiration_researches += 1;

                while low_score <= alpha {
                    alpha = alpha
                        .saturating_sub(ASPIRATION_WINDOW_OFFSETS[alpha_window_index])
                        .min(-i16::MAX);
                    alpha_window_index += 1;
                }
            } else if high_score >= beta {
                self.stats.aspiration_researches += 1;

                while high_score >= beta {
                    beta = beta.saturating_add(ASPIRATION_WINDOW_OFFSETS[beta_window_index]);
                    beta_window_index += 1;
                }
            } else {
                break;
            }
        }

        if pv.is_empty() {
            error!("PV is empty in alpha_beta_init");
            panic!("PV is empty in alpha_beta_init")
        }

        self.single_root_move
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
        debug_assert!(alpha <= beta);

        self.stats.current_iteration_total_nodes += 1;

        if ply != 0
            && (RepetitionTracker::test_repetition(self.board)
                || self.board.is_insufficient_material())
        {
            parent_pv.clear();
            return Ok(self.eval_draw());
        }

        if in_check {
            draft += 1;
        }

        if draft == 0 {
            self.stats.total_search_leaves += 1;

            if self.stats.total_search_leaves % 16384 == 16383 {
                let stop_received = matches!(self.stop_rx.try_recv(), Ok(()));
                if stop_received
                    || self.hard_cutoff_time.is_some_and(|t| Instant::now() >= t)
                    || self.stats.current_iteration_total_nodes >= self.max_nodes_for_current_iteration
                {
                    if stop_received {
                        self.stop_received = stop_received;
                    }

                    return Err(());
                }
            }

            parent_pv.clear();
            return Ok(self
                .board
                .quiescense_side_to_move_relative(alpha, beta, 255, &DEFAULT_PARAMS, &mut self.rollback)
                .0);
        }

        // When at the root and with multi-pv enabled then this will be the lowest PV score
        let mut best_score = i16::MIN;
        // When at the root and with multi-pv enabled then this may not be the actual best move
        let mut best_move = None;
        let mut new_killers = [EMPTY_MOVE, EMPTY_MOVE];

        let is_pv = alpha + 1 != beta;

        let mut move_gen = MoveGenerator::new();
        let tt_entry = self
            .transposition_table
            .get_entry(self.board.hash, self.starting_fullmove);
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

            move_gen.set_tt_move(tt_data.important_move);
        }

        let mut pv: TinyVec<[Move; 32]> = tiny_vec!();

        let mut futility_prune = false;
        if draft < 4 && !is_pv && !in_check && alpha.abs() < 2000 && beta.abs() < 2000 {
            let eval = self.board.evaluate_side_to_move_relative(&DEFAULT_PARAMS);

            // Reverse futility pruning
            if eval - 120 - 128 * (draft - 1) as i16 >= beta {
                return Ok((eval + beta) / 2);
            }

            futility_prune = (eval + 307 + 173 * (draft - 1) as i16) < alpha;

            // Razoring
            if (eval + 332 + 279 * (draft - 1) as i16) < alpha {
                let score = self.board.quiescense_side_to_move_relative(alpha, beta, 255, &DEFAULT_PARAMS, &mut self.rollback).0;
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
            && tt_entry.is_none_or(|e| e.move_type != MoveType::FailLow && e.get_score(ply) >= beta)
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
        if tt_entry.is_none() && is_pv && draft > 5 {
            let mut iid_pv = tiny_vec!();
            self.alpha_beta_recurse(
                alpha,
                beta,
                draft - 2,
                ply,
                killers,
                in_check,
                can_null_move,
                &mut iid_pv,
            )?;

            let tt_entry = self
                .transposition_table
                .get_entry(self.board.hash, self.starting_fullmove);
            if let Some(tt_data) = tt_entry {
                move_gen.set_tt_move(tt_data.important_move);
            }
        }

        if !in_check {
            move_gen.generate_moves_pseudo_legal(self.board);
        } else {
            move_gen.generate_moves_check_evasion(
                self.board,
                Some(self.history_table),
                Some(&self.move_history),
                Some(self.continuation_history),
                Some(killers),
            );
        }

        let mut searched_quiet_moves: TinyVec<[Move; 64]> = tiny_vec!();
        let mut searched_moves = 0;
        let mut has_legal_move = false;
        let mut improved_alpha = false;
        loop {
            let r#move = match move_gen.get_next_move() {
                GetMoveResult::Move(scored_move) => scored_move,
                GetMoveResult::GenerateMoves => {
                    move_gen.generate_more_moves(
                        self.board,
                        Some(self.history_table),
                        Some(&self.move_history),
                        Some(self.continuation_history),
                        Some(killers),
                    );
                    continue;
                }
                GetMoveResult::NoMoves => break,
            };

            if !is_pv && r#move.m.data & MOVE_FLAG_CAPTURE_FULL != 0 {
                let see_margin = draft as i16 * -50;
                if !self.board.is_static_exchange_eval_at_least(r#move.m, see_margin) {
                    continue;
                }
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

            let start_of_search_nodes = self.stats.current_iteration_total_nodes;

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

                if ply == 0 {
                    let pv_data = PvData {
                        pv: TinyVec::default(),
                        search_result: SearchResult {
                            best_move: r#move.m,
                            score,
                        },
                    };

                    if self.root_pvs.len() == self.multi_pv as usize {
                        self.root_pvs.pop_back();
                    }

                    let index = self.root_pvs.partition_point(|pv| pv.search_result.score > score);
                    self.root_pvs.insert(index, pv_data);
                }

                self.transposition_table.store_entry(TTEntry::new(
                    self.board.hash,
                    r#move.m,
                    MoveType::FailHigh,
                    score,
                    draft,
                    ply,
                    self.starting_fullmove,
                ));

                return Ok(score);
            }

            if score > best_score {
                if ply == 0 {
                    *parent_pv = pv.clone();
                    parent_pv.push(r#move.m);

                    let pv_data = PvData {
                        pv: parent_pv.clone(),
                        search_result: SearchResult {
                            best_move: r#move.m,
                            score,
                        },
                    };

                    if self.root_pvs.len() == self.multi_pv as usize {
                        self.root_pvs.pop_back();
                    }

                    let index = self.root_pvs.partition_point(|pv| pv.search_result.score > score);
                    self.root_pvs.insert(index, pv_data);

                    // If there is a new best PV
                    if index == 0 {
                        self.root_pv_branch_nodes = self.stats.current_iteration_total_nodes - start_of_search_nodes;
                    }

                    score = self.root_pvs.back().unwrap().search_result.score;
                }

                if ply != 0 || self.root_pvs.len() == self.multi_pv as usize {
                    best_score = score;
                    best_move = Some(r#move.m);
                    if score > alpha {
                        alpha = score;
                        improved_alpha = true;

                        // This will be handled separately for root nodes
                        if is_pv && ply != 0 {
                            *parent_pv = pv.clone();
                            parent_pv.push(r#move.m);
                        }
                    }
                }
            }

            // TODO: change to check for capture flag
            if r#move.m.flags() == 0 {
                searched_quiet_moves.push(r#move.m);
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
                return Ok(self.eval_draw());
            }
        } else if searched_moves == 1 && ply == 0 {
            self.single_root_move = true;
        }

        if ply == 0 && self.multi_pv > 1 {
            let best_pv = self
                .root_pvs
                .front()
                .expect("No root pv found at the end of alpha_beta_recurse for ply == 0");
            best_move = Some(best_pv.search_result.best_move);
            best_score = best_pv.search_result.score;
            *parent_pv = best_pv.pv.clone();
            improved_alpha |= best_score > alpha;
        }

        let entry_type = if improved_alpha {
            MoveType::Best
        } else {
            MoveType::FailLow
        };
        self.transposition_table.store_entry(TTEntry::new(
            self.board.hash,
            best_move.unwrap(),
            entry_type,
            best_score,
            draft,
            ply,
            self.starting_fullmove,
        ));

        Ok(best_score)
    }

    fn apply_history_to_move_scores(&mut self, moves: &mut ArrayVec<ScoredMove, MOVE_ARRAY_SIZE>) {
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

    fn eval_draw(&self) -> i16 {
        self.contempt * if self.board.white_to_move ^ self.white_started_search { 1 } else { -1 }
    }
}

impl Board {
    pub fn quiescense_side_to_move_relative(
        &mut self,
        mut alpha: i16,
        beta: i16,
        draft: u8,
        params: &EvalParams,
        rollback: &mut MoveRollback,
    ) -> (i16, Board) {
        if self.is_insufficient_material() {
            return (0, self.clone());
        }

        let mut best_score;
        let in_check = self.is_in_check(false);
        if !in_check {
            let stand_pat = self.evaluate_side_to_move_relative(params);

            if stand_pat >= beta {
                return (stand_pat, self.clone());
            }

            if self.game_stage > ENDGAME_GAME_STAGE_FOR_QUIESCENSE
                && stand_pat
                    + params[FeatureIndex::PieceValues as usize + PIECE_QUEEN as usize]
                    + 100
                    // maybe this shouldn't include quiet promotions because those would already be covered under the standard margin
                    + if self.can_probably_promote() {
                        params[FeatureIndex::PieceValues as usize + PIECE_QUEEN as usize] - 100
                    } else {
                        0
                    }
                    < alpha
            {
                return (stand_pat, self.clone());
            }

            if alpha < stand_pat {
                alpha = stand_pat;
            }

            best_score = stand_pat;
        } else {
            best_score = -i16::MAX;
        }

        let mut best_position = None;

        let mut moves = ArrayVec::new();
        if in_check {
            self.generate_pseudo_legal_check_evasions(&DEFAULT_HISTORY_TABLE, &mut moves);
        } else {
            self.generate_pseudo_legal_capture_moves(&mut moves);
        }

        for move_index in 0..moves.len() {
            select_next_move(&mut moves, move_index);
            let r#move = &moves[move_index];

            let (legal, move_made) = self.test_legality_and_maybe_make_move(r#move.m, rollback);
            if !legal {
                if move_made {
                    self.unmake_move(&r#move.m, rollback);
                }

                continue;
            }

            // Only doing captures right now so not checking halfmove or threefold repetition here
            let (result, pos) = self.quiescense_side_to_move_relative(-beta, -alpha, draft - 1, params, rollback);
            let result = -result;

            self.unmake_move(&r#move.m, rollback);

            if result >= beta {
                return (result, pos);
            }

            if best_score < result {
                best_score = result;
                best_position = Some(pos);

                if alpha < result {
                    alpha = result;
                }
            }
        }

        (
            best_score,
            if best_position.is_some() {
                best_position.unwrap()
            } else {
                self.clone()
            },
        )
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

#[inline]
fn select_next_move(moves: &mut ArrayVec<ScoredMove, MOVE_ARRAY_SIZE>, index_to_skip: usize) {
    // Perform one iteration of selection sort every time another move needs to be evaluated
    let mut best_move_score = moves[index_to_skip].score;
    let mut best_move_index = index_to_skip;

    for sort_index in (index_to_skip + 1)..moves.len() {
        if moves[sort_index].score > best_move_score {
            best_move_score = moves[sort_index].score;
            best_move_index = sort_index;
        }
    }

    moves.swap(index_to_skip, best_move_index);
}
