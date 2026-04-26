use std::{collections::VecDeque, sync::mpsc::Receiver, time::Instant};

use arrayvec::ArrayVec;
use log::{debug, error};
use tinyvec::{TinyVec, tiny_vec};
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_KING, PIECE_MASK, PIECE_PAWN, PIECE_QUEEN},
    eval_values::CENTIPAWN_VALUES_MIDGAME,
    evaluate::{ENDGAME_GAME_STAGE_FOR_QUIESCENSE, MATE_THRESHOLD},
    move_generator::{
        MOVE_ARRAY_SIZE, MOVE_SCORE_CONT_HISTORY_PLY1_MAX, MOVE_SCORE_CONT_HISTORY_PLY2_MAX, MOVE_SCORE_HISTORY_MAX,
        ScoredMove,
    },
    move_generator_struct::{GetMoveResult, MoveGenerator},
    moves::{MOVE_FLAG_CAPTURE, MOVE_FLAG_CAPTURE_FULL, MOVE_FLAG_PROMOTION, MOVE_FLAG_PROMOTION_FULL, Move},
    repetition_tracker::RepetitionTracker,
    time_management::{get_cutoff_times, modify_cutoff_time},
    transposition_table::{self, MoveType, TTEntry, TranspositionTable},
    uci::UciInterface,
    uci_required_options_helper::RequiredUciOptions,
};

pub type HistoryTable = [[[i16; 64]; 6]; 2];
pub static DEFAULT_HISTORY_TABLE: HistoryTable = [[[0; 64]; 6]; 2];

pub type ContinuationHistoryTable = [[[[[i16; 64]; 6]; 64]; 6]; 2];
pub type MutRelevantContinuationHistories<'a> = [Option<&'a mut [[i16; 64]; 6]>; 2];
pub type RelevantContinuationHistories<'a> = [Option<&'a [[i16; 64]; 6]>; 2];

/// Each value is in addition to the last
static ASPIRATION_WINDOW_OFFSETS: [i16; 4] = [50, 150, 600, i16::MAX];

pub const EMPTY_MOVE: Move = Move { data: 0 };

include!(concat!(env!("OUT_DIR"), "/ln_fixedpoint_128_values.rs"));

pub struct ContinuationHistoryTables {
    pub ply1: ContinuationHistoryTable,
    pub ply2: ContinuationHistoryTable,
}

#[derive(Default)]
pub struct SearchStats {
    pub depth: u8,
    pub current_iteration_total_nodes: u64,
    pub previous_iterations_total_nodes: u64,
    pub total_search_leaves: u64,
    pub aspiration_researches: u8,
    pub selective_depth: u8,
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
    pub selective_depth: u8,
}

pub struct SearchStack {
    mov: Move,
    moved_piece_type: u8,
    excluded_move: Option<Move>,
    killers: [Move; 2],
}

pub struct Searcher<'a> {
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
    continuation_histories: &'a mut ContinuationHistoryTables,
    multi_pv: u8,
    root_pvs: VecDeque<PvData>,
    extra_uci_options: RequiredUciOptions,
    root_pv_branch_nodes: u64,
    pv_nodes_fractions: VecDeque<f32>,
    contempt: i16,
    white_started_search: bool,
    ss: Vec<SearchStack>,
    root_killers: [Move; 2],
    repetitions: &'a mut RepetitionTracker,
}

impl<'a> Searcher<'a> {
    pub fn new(
        transposition_table: &'a mut TranspositionTable,
        history_table: &'a mut HistoryTable,
        stop_rx: &'a Receiver<()>,
        continuation_histories: &'a mut ContinuationHistoryTables,
        multi_pv: u8,
        extra_uci_options: RequiredUciOptions,
        contempt: i16,
        repetitions: &'a mut RepetitionTracker,
    ) -> Self {
        assert!(multi_pv >= 1);

        Self {
            stats: SearchStats::default(),
            transposition_table,
            history_table,
            starting_fullmove: 0xFF,
            hard_cutoff_time: None,
            single_root_move: false,
            starting_in_check: true,
            stop_rx,
            stop_received: false,
            total_max_nodes: u64::MAX,
            max_nodes_for_current_iteration: u64::MAX,
            continuation_histories,
            multi_pv,
            root_pvs: VecDeque::with_capacity(multi_pv as usize),
            extra_uci_options,
            root_pv_branch_nodes: 0,
            pv_nodes_fractions: VecDeque::new(),
            contempt,
            white_started_search: true,
            ss: Vec::new(),
            root_killers: [EMPTY_MOVE; 2],
            repetitions,
        }
    }

    pub fn iterative_deepening_search(
        &mut self,
        mut board: Board,
        time: &Option<UciTimeControl>,
        search: &Option<UciSearchControl>,
    ) -> SearchResult {
        let start_time = Instant::now();
        let mut soft_cutoff_time = None;
        let mut search_control: SearchControl;
        let mut max_depth = 40;
        let mut cutoff_times = None;

        // Don't forget to set these if search is started some other way
        self.starting_fullmove = board.fullmove_counter as u8;
        self.starting_in_check = board.is_in_check(false);
        self.white_started_search = board.white_to_move;

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
                    let (time_left, increment) = if board.white_to_move {
                        (white_time, white_increment)
                    } else {
                        (black_time, black_increment)
                    };

                    cutoff_times = Some(get_cutoff_times(
                        time_left,
                        increment,
                        &start_time,
                        board.fullmove_counter,
                    ));
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
            let end_search = self.alpha_beta_init(&mut board, depth, latest_result.clone());
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
                        pv.selective_depth,
                    );
                }

                self.stats.aspiration_researches = 0;

                if self.stop_received
                    || (search_control != SearchControl::Infinite
                        && (end_search || worst_pv.search_result.score.abs() >= MATE_THRESHOLD || depth >= max_depth))
                {
                    return best_pv.search_result.clone();
                }

                if matches!(search_control, SearchControl::Time) {
                    if let Some(cutoff_times) = &cutoff_times {
                        let pv_nodes_fraction =
                            self.root_pv_branch_nodes as f32 / self.stats.current_iteration_total_nodes as f32;

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

    fn alpha_beta_init(&mut self, board: &mut Board, draft: u8, last_result: Option<SearchResult>) -> bool {
        self.stats.depth = draft;
        self.single_root_move = false;
        self.root_killers = [EMPTY_MOVE; 2];

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
            self.ss.clear();
            self.root_pvs.clear();
            self.stats.previous_iterations_total_nodes += self.stats.current_iteration_total_nodes;
            self.stats.current_iteration_total_nodes = 0;
            self.max_nodes_for_current_iteration = self.total_max_nodes - self.stats.previous_iterations_total_nodes;

            let result = self.alpha_beta_recurse(board, alpha, beta, draft, 0, self.starting_in_check, true, &mut pv);

            if result.is_err() {
                self.root_pvs.clear();

                // In this case the state of the board will not been reset back to the starting state
                return true;
            }

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
        board: &mut Board,
        mut alpha: i16,
        beta: i16,
        mut draft: u8,
        ply: u8,
        in_check: bool,
        can_null_move: bool,
        parent_pv: &mut TinyVec<[Move; 32]>,
    ) -> Result<i16, ()> {
        debug_assert!(alpha <= beta);

        self.stats.current_iteration_total_nodes += 1;

        if ply != 0
            && (board.halfmove_clock >= 100
                || self.repetitions.test_repetition(board)
                || board.is_insufficient_material())
        {
            parent_pv.clear();
            return Ok(self.eval_draw(board));
        }

        self.stats.selective_depth = self.stats.selective_depth.max(ply);

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

            return Ok(self.quiescense_side_to_move_relative(board, alpha, beta, ply + 1));
        }

        if self.ss.get(ply as usize).is_none() {
            self.ss.push(SearchStack::new());
        } else {
            self.ss[ply as usize].killers = [EMPTY_MOVE; 2];
        }

        // When at the root and with multi-pv enabled then this will be the lowest PV score
        let mut best_score = i16::MIN;
        // When at the root and with multi-pv enabled then this may not be the actual best move
        let mut best_move = None;

        let is_pv = alpha + 1 != beta;

        let mut move_gen = MoveGenerator::new();
        let excluded_move = self.ss[ply as usize].excluded_move.clone();
        let tt_entry = if excluded_move.is_none() {
            self.transposition_table.get_entry(board.hash, self.starting_fullmove)
        } else {
            None
        };
        if let Some(tt_data) = tt_entry {
            if !is_pv && tt_data.draft >= draft {
                let tt_score = tt_data.get_score(ply);

                match tt_data.move_type {
                    transposition_table::MoveType::FailHigh => {
                        if tt_score >= beta {
                            // should history be updated here?
                            update_killers_and_history(
                                &board,
                                &tt_data.important_move,
                                draft,
                                ply,
                                &mut self.history_table,
                                &mut self.continuation_histories,
                                &mut self.ss,
                                &mut self.root_killers,
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
        if draft < 6 && !is_pv && !in_check && alpha.abs() < 2000 && beta.abs() < 2000 && excluded_move.is_none() {
            let eval = board.evaluate_side_to_move_relative();

            // Reverse futility pruning
            if eval - (90 * draft as i16) >= beta {
                return Ok((eval + beta) / 2);
            }

            if draft < 4 {
                futility_prune = (eval + 307 + 173 * (draft - 1) as i16) < alpha;

                // Razoring
                if (eval + 332 + 279 * (draft - 1) as i16) < alpha {
                    let score = self.quiescense_side_to_move_relative(board, alpha, beta, ply + 1);
                    if score < alpha {
                        return Ok(score);
                    }
                }
            }
        }

        // Null move pruning
        let our_side = if board.white_to_move { 0 } else { 1 };
        if can_null_move
            && !is_pv
            && beta < i16::MAX
            && draft > 4
            && !in_check
            && excluded_move.is_none()
            && tt_entry.is_none_or(|e| e.move_type != MoveType::FailLow && e.get_score(ply) >= beta)
            && board.piece_bitboards[our_side][PIECE_PAWN as usize]
                | board.piece_bitboards[our_side][PIECE_KING as usize]
                != board.side_occupancy[our_side]
        {
            let en_passant_target_square_index = board.make_null_move();
            self.ss[ply as usize].mov = EMPTY_MOVE;
            let saved_killers = self.ss[ply as usize].killers;
            self.ss[ply as usize].killers = [EMPTY_MOVE; 2];

            // Need to ensure draft >= 1
            let reduction = 3 + draft / 6;

            let nmp_score = -self.alpha_beta_recurse(
                board,
                -beta,
                -(beta - 1),
                draft - reduction,
                ply + 1,
                false,
                false,
                &mut pv,
            )?;

            board.unmake_null_move(en_passant_target_square_index);
            self.ss[ply as usize].killers = saved_killers;

            if nmp_score >= beta {
                return Ok(nmp_score);
            }
        }

        // Internal Iterative Deepening
        if tt_entry.is_none() && is_pv && draft > 5 {
            let mut iid_pv = tiny_vec!();
            self.alpha_beta_recurse(board, alpha, beta, draft - 2, ply, in_check, can_null_move, &mut iid_pv)?;

            self.ss[ply as usize].killers = [EMPTY_MOVE; 2];

            let tt_entry = self.transposition_table.get_entry(board.hash, self.starting_fullmove);
            if let Some(tt_data) = tt_entry {
                move_gen.set_tt_move(tt_data.important_move);
            }
        }

        if !in_check {
            move_gen.generate_moves_pseudo_legal(board);
        } else {
            let last_move = if ply > 0 {
                self.ss[ply as usize - 1].mov
            } else {
                EMPTY_MOVE
            };
            let killers = if ply > 0 {
                &self.ss[ply as usize - 1].killers
            } else {
                &self.root_killers
            };
            move_gen.generate_moves_check_evasion(
                board,
                Some(self.history_table),
                Some(&self.ss),
                Some(self.continuation_histories),
                Some(killers),
                Some(ply),
            );
        }

        let mut searched_quiet_moves: TinyVec<[Move; 64]> = tiny_vec!();
        let mut searched_moves = 0;
        let mut has_legal_move = false;
        let mut improved_alpha = false;
        let see_margin = (draft as i16).pow(2) * -16;
        loop {
            let r#move = match move_gen.get_next_move() {
                GetMoveResult::Move(scored_move) => scored_move,
                GetMoveResult::GenerateMoves => {
                    let last_move = if ply > 0 {
                        self.ss[ply as usize - 1].mov
                    } else {
                        EMPTY_MOVE
                    };
                    let killers = if ply > 0 {
                        &self.ss[ply as usize - 1].killers
                    } else {
                        &self.root_killers
                    };
                    move_gen.generate_more_moves(
                        board,
                        Some(self.history_table),
                        Some(&self.ss),
                        Some(self.continuation_histories),
                        Some(killers),
                        Some(ply),
                    );
                    continue;
                }
                GetMoveResult::NoMoves => break,
            };

            if excluded_move.is_some_and(|em| em == r#move.m) {
                continue;
            }

            if !is_pv && r#move.m.data & MOVE_FLAG_CAPTURE_FULL != 0 {
                if !board.is_static_exchange_eval_at_least(r#move.m, see_margin) {
                    continue;
                }
            }

            let mut new_board = board.clone();
            let (legal, move_made) = new_board.test_legality_and_maybe_make_move(r#move.m, self.repetitions);
            if !legal {
                if move_made {
                    self.repetitions.unmake_move(new_board.hash);
                }
                continue;
            }

            has_legal_move = true;
            let gives_check = new_board.is_in_check(false);

            // Futility pruning and late move pruning
            if (futility_prune
                || (!is_pv && !in_check && searched_moves >= 6 && r#move.score < 51 + (-92 * draft as i16)))
                && searched_moves >= 1
                && !gives_check
                && r#move.m.data & (MOVE_FLAG_CAPTURE_FULL | MOVE_FLAG_PROMOTION_FULL) == 0
            {
                self.repetitions.unmake_move(new_board.hash);
                continue;
            }

            let mut extension = 0;
            if ply != 0
                && draft >= 5
                && excluded_move.is_none()
                && tt_entry.is_some_and(|tt_entry| {
                    tt_entry.important_move == r#move.m
                        && tt_entry.draft >= draft - 3
                        && tt_entry.move_type != MoveType::FailLow
                        && tt_entry.get_score(ply).abs() < MATE_THRESHOLD - 2600
                })
            {
                self.repetitions.unmake_move(new_board.hash);
                self.ss[ply as usize].excluded_move = Some(r#move.m);

                let verification_draft = draft / 2;
                let verification_beta = tt_entry.unwrap().get_score(ply) - draft as i16 * 2;
                let beginning_nodes = self.stats.current_iteration_total_nodes;

                let verification_score = self.alpha_beta_recurse(
                    board,
                    verification_beta - 1,
                    verification_beta,
                    verification_draft,
                    ply,
                    in_check,
                    can_null_move,
                    parent_pv,
                )?;

                self.ss[ply as usize].killers = [EMPTY_MOVE; 2];

                // If there is only one move
                if self.stats.current_iteration_total_nodes == beginning_nodes + 1 {
                    extension = 1;
                } else if verification_score < verification_beta {
                    extension = 1;
                }

                self.ss[ply as usize].excluded_move = None;
                self.repetitions.make_move(r#move.m, new_board.hash);
            }

            self.ss[ply as usize].mov = r#move.m;
            self.ss[ply as usize].moved_piece_type = new_board.get_piece_64(r#move.m.to() as usize) & PIECE_MASK;

            // Late move reduction
            let reduction_ply = if draft > 2 && searched_moves > 3 {
                let flags = r#move.m.flags();

                // Using formula and values from Ethereal according to https://www.chessprogramming.org/Late_Move_Reductions
                let mut reduction_fixedpoint_128 = if flags & MOVE_FLAG_CAPTURE == 0 && flags & MOVE_FLAG_PROMOTION == 0
                {
                    (100 + LN_FIXEDPOINT_128_VALUES[draft.min(LN_FIXEDPOINT_128_VALUES.len() as u8 - 1) as usize]
                        as i32
                        * LN_FIXEDPOINT_128_VALUES
                            [searched_moves.min(LN_FIXEDPOINT_128_VALUES.len() as u8 - 1) as usize]
                            as i32
                        / 316) as i16
                } else {
                    3 * 128
                };

                if is_pv {
                    reduction_fixedpoint_128 -= 128;
                }

                if in_check {
                    reduction_fixedpoint_128 -= 128;
                }

                let rounding = (reduction_fixedpoint_128 % 128 >= 64) as i16;
                ((reduction_fixedpoint_128 / 128) + rounding).clamp(0, draft as i16 - 1) as u8
            } else {
                0
            };

            let start_of_search_nodes = self.stats.current_iteration_total_nodes;
            if ply == 0 {
                self.stats.selective_depth = 0;
            }

            let mut score;
            if searched_moves == 0 || ply == 0 {
                // Use reduction
                score = -self.alpha_beta_recurse(
                    &mut new_board,
                    -beta,
                    -alpha,
                    draft - reduction_ply - 1 + extension,
                    ply + 1,
                    gives_check,
                    can_null_move,
                    &mut pv,
                )?;

                if score > alpha && reduction_ply > 0 {
                    // Do a full search
                    score = -self.alpha_beta_recurse(
                        &mut new_board,
                        -beta,
                        -alpha,
                        draft - 1 + extension,
                        ply + 1,
                        gives_check,
                        can_null_move,
                        &mut pv,
                    )?;
                }
            } else {
                // Use null window and reduction
                score = -self.alpha_beta_recurse(
                    &mut new_board,
                    -alpha - 1,
                    -alpha,
                    draft - reduction_ply - 1 + extension,
                    ply + 1,
                    gives_check,
                    can_null_move,
                    &mut pv,
                )?;

                if score > alpha {
                    // Do a full search
                    score = -self.alpha_beta_recurse(
                        &mut new_board,
                        -beta,
                        -alpha,
                        draft - 1 + extension,
                        ply + 1,
                        gives_check,
                        can_null_move,
                        &mut pv,
                    )?;
                }
            }

            self.repetitions.unmake_move(new_board.hash);
            searched_moves += 1;

            if score >= beta {
                let mut relevant_cont_histories = update_killers_and_history(
                    &board,
                    &r#move.m,
                    draft,
                    ply,
                    &mut self.history_table,
                    &mut self.continuation_histories,
                    &mut self.ss,
                    &mut self.root_killers,
                );

                let penalty = -(draft as i16) * (draft as i16);
                for m in searched_quiet_moves {
                    update_history(
                        &board,
                        &mut self.history_table,
                        &m,
                        penalty,
                        &mut relevant_cont_histories,
                    );
                }

                if ply == 0 {
                    let pv_data = PvData {
                        pv: TinyVec::default(),
                        search_result: SearchResult {
                            best_move: r#move.m,
                            score,
                        },
                        selective_depth: self.stats.selective_depth,
                    };

                    if self.root_pvs.len() == self.multi_pv as usize {
                        self.root_pvs.pop_back();
                    }

                    let index = self.root_pvs.partition_point(|pv| pv.search_result.score > score);
                    self.root_pvs.insert(index, pv_data);
                }

                if excluded_move.is_none() {
                    self.transposition_table.store_entry(TTEntry::new(
                        board.hash,
                        r#move.m,
                        MoveType::FailHigh,
                        score,
                        draft,
                        ply,
                        self.starting_fullmove,
                    ));
                }

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
                        selective_depth: self.stats.selective_depth,
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
                    board
                );
                panic!("Found no legal moves from the root of the search")
            } else if in_check {
                parent_pv.clear();
                return Ok(board.evaluate_checkmate_side_to_move_relative(ply));
            } else {
                parent_pv.clear();
                return Ok(self.eval_draw(board));
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

        if excluded_move.is_none() {
            let entry_type = if improved_alpha {
                MoveType::Best
            } else {
                MoveType::FailLow
            };
            self.transposition_table.store_entry(TTEntry::new(
                board.hash,
                best_move.unwrap(),
                entry_type,
                best_score,
                draft,
                ply,
                self.starting_fullmove,
            ));
        }

        Ok(best_score)
    }

    pub fn quiescense_side_to_move_relative(&mut self, board: &mut Board, mut alpha: i16, beta: i16, ply: u8) -> i16 {
        self.stats.current_iteration_total_nodes += 1;

        if board.is_insufficient_material() {
            return 0;
        }

        let is_pv = alpha + 1 != beta;
        let mut moves = ArrayVec::new();
        let tt_entry = self.transposition_table.get_entry(board.hash, self.starting_fullmove);
        if let Some(tt_data) = tt_entry {
            let tt_eval = tt_data.get_score(ply);

            if !is_pv {
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
            }

            if tt_data.important_move.data & MOVE_FLAG_CAPTURE_FULL != 0 {
                moves.push(ScoredMove {
                    m: tt_data.important_move,
                    score: 1,
                });
            }
        }

        let mut best_score;
        let in_check = board.is_in_check(false);
        if !in_check {
            let stand_pat = board.evaluate_side_to_move_relative();

            if stand_pat >= beta {
                return stand_pat;
            }

            if board.game_stage > ENDGAME_GAME_STAGE_FOR_QUIESCENSE
                && stand_pat + CENTIPAWN_VALUES_MIDGAME[PIECE_QUEEN as usize] + 100 < alpha
            {
                return stand_pat;
            }

            if alpha < stand_pat {
                alpha = stand_pat;
            }

            best_score = stand_pat;
        } else {
            best_score = -i16::MAX;
        }
        let mut best_move = None;
        let mut improved_alpha = false;

        // Round 0 is the tt move, round 1 is regular move gen
        for round in 0..2 {
            for move_index in 0..moves.len() {
                select_next_move(&mut moves, move_index);
                let r#move = &moves[move_index];

                if round == 1 && tt_entry.is_some_and(|v| v.important_move == r#move.m) {
                    continue;
                }

                // SEE is coded to be run before the move is made so have to do it before testing legality
                if !board.is_static_exchange_eval_at_least(r#move.m, 0) {
                    continue;
                }

                let mut new_board = board.clone();
                let (legal, move_made) = new_board.test_legality_and_maybe_make_move(r#move.m, self.repetitions);
                if !legal {
                    if move_made {
                        self.repetitions.unmake_move(new_board.hash);
                    }
                    continue;
                }

                // Only doing captures right now so not checking halfmove or threefold repetition here
                let score = -self.quiescense_side_to_move_relative(&mut new_board, -beta, -alpha, ply + 1);

                self.repetitions.unmake_move(new_board.hash);

                if score >= beta {
                    self.transposition_table.store_entry(TTEntry::new(
                        board.hash,
                        r#move.m,
                        MoveType::FailHigh,
                        score,
                        0,
                        0,
                        self.starting_fullmove,
                    ));

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
                if in_check {
                    board.generate_pseudo_legal_check_evasions(self.history_table, &mut moves);
                } else {
                    board.generate_pseudo_legal_capture_moves(&mut moves);
                }
            }
        }

        if let Some(bm) = best_move {
            let entry_type = if improved_alpha {
                MoveType::Best
            } else {
                MoveType::FailLow
            };
            self.transposition_table.store_entry(TTEntry::new(
                board.hash,
                bm,
                entry_type,
                best_score,
                0,
                ply,
                self.starting_fullmove,
            ));
        } else if best_score == -i16::MAX {
            best_score = alpha.max(-4000);
        }

        best_score
    }

    fn eval_draw(&self, board: &Board) -> i16 {
        self.contempt
            * if board.white_to_move ^ self.white_started_search {
                1
            } else {
                -1
            }
    }
}

#[inline]
fn update_killers_and_history<'a>(
    board: &Board,
    m: &Move,
    draft: u8,
    ply: u8,
    history_table: &mut HistoryTable,
    continuation_histories: &'a mut ContinuationHistoryTables,
    ss: &mut Vec<SearchStack>,
    root_killers: &mut [Move; 2],
) -> MutRelevantContinuationHistories<'a> {
    let mut relevant_cont_histories = get_relevant_cont_histories(ss, board, continuation_histories, ply as usize);

    // TODO: Change this to check for capture flag specifically
    if m.flags() != 0 {
        return relevant_cont_histories;
    }

    let killers = if ply > 0 {
        &mut ss[ply as usize - 1].killers
    } else {
        root_killers
    };

    update_history(
        board,
        history_table,
        m,
        (draft as i16) * (draft as i16),
        &mut relevant_cont_histories,
    );

    if killers[0] != *m {
        if killers[1] == *m {
            (killers[0], killers[1]) = (killers[1], killers[0]);
        } else {
            killers[1] = *m;
        }
    }

    return relevant_cont_histories;
}

#[inline]
fn update_history(
    board: &Board,
    history_table: &mut HistoryTable,
    m: &Move,
    bonus: i16,
    relevant_cont_histories: &mut MutRelevantContinuationHistories,
) {
    // from https://www.chessprogramming.org/History_Heuristic
    let piece_type_index = (board.get_piece_64(m.from() as usize) & PIECE_MASK) as usize - 1;
    let history_color_value = if board.white_to_move { 0 } else { 1 };
    let to = m.to() as usize;

    let clamped_history_bonus = (bonus as i32).clamp(-MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_HISTORY_MAX);
    let current_history = &mut history_table[history_color_value][piece_type_index][to];
    *current_history += (clamped_history_bonus
        - ((*current_history as i32) * clamped_history_bonus.abs() / MOVE_SCORE_HISTORY_MAX))
        as i16;

    if let Some(relevant_cont_hist) = &mut relevant_cont_histories[0] {
        let clamped_const_history_bonus =
            (bonus as i32).clamp(-MOVE_SCORE_CONT_HISTORY_PLY1_MAX, MOVE_SCORE_CONT_HISTORY_PLY1_MAX);
        let current_cont_hist = &mut relevant_cont_hist[piece_type_index][to];
        *current_cont_hist += (clamped_const_history_bonus
            - ((*current_cont_hist as i32) * clamped_const_history_bonus.abs() / MOVE_SCORE_CONT_HISTORY_PLY1_MAX))
            as i16;
    }

    if let Some(relevant_cont_hist) = &mut relevant_cont_histories[1] {
        let clamped_const_history_bonus =
            ((bonus / 2) as i32).clamp(-MOVE_SCORE_CONT_HISTORY_PLY2_MAX, MOVE_SCORE_CONT_HISTORY_PLY2_MAX);
        let current_cont_hist = &mut relevant_cont_hist[piece_type_index][to];
        *current_cont_hist += (clamped_const_history_bonus
            - ((*current_cont_hist as i32) * clamped_const_history_bonus.abs() / MOVE_SCORE_CONT_HISTORY_PLY2_MAX))
            as i16;
    }
}

#[inline]
fn get_relevant_cont_histories<'a>(
    ss: &Vec<SearchStack>,
    board: &Board,
    continuation_histories: &'a mut ContinuationHistoryTables,
    ply: usize,
) -> MutRelevantContinuationHistories<'a> {
    let mut result = [None, None];
    let side = if board.white_to_move { 0 } else { 1 };

    let table_for_ply = &mut continuation_histories.ply1;
    let ply_offset = 1;
    if ply >= ply_offset {
        let entry = &ss[ply - ply_offset];
        if entry.mov != EMPTY_MOVE {
            result[0] = Some(&mut table_for_ply[side][entry.moved_piece_type as usize - 1][entry.mov.to() as usize]);
        }
    }

    // I think I have to manually repeat this code or run afoul of the rules against mutably borrowing something multiple times
    let table_for_ply = &mut continuation_histories.ply2;
    let ply_offset = 2;
    if ply >= ply_offset {
        let entry = &ss[ply - ply_offset];
        if entry.mov != EMPTY_MOVE {
            result[1] = Some(&mut table_for_ply[side][entry.moved_piece_type as usize - 1][entry.mov.to() as usize]);
        }
    }

    result
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

impl SearchStack {
    pub fn new() -> Self {
        Self {
            mov: EMPTY_MOVE,
            moved_piece_type: 0xFF,
            excluded_move: None,
            killers: [EMPTY_MOVE; 2],
        }
    }

    pub fn get_mov(&self) -> Move {
        self.mov
    }

    pub fn get_moved_piece_type(&self) -> u8 {
        self.moved_piece_type
    }
}
