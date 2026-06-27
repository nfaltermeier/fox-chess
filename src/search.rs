use std::{
    collections::VecDeque,
    sync::{atomic::AtomicBool, mpsc::Receiver},
    thread,
    time::{Duration, Instant},
};

use arrayvec::ArrayVec;
use log::{debug, error};
use tinyvec::{TinyVec, tiny_vec};
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_KING, PIECE_MASK, PIECE_PAWN},
    evaluate::{MATE_THRESHOLD, MATE_VALUE},
    history::{
        ContinuationHistoryTables, CorrectionHistoryTables, HistoryTable, ThreadHistoryTables, update_histories,
        update_killers_and_histories,
    },
    move_generator::{MOVE_ARRAY_SIZE, ScoredMove},
    moves::Move,
    nnue::{AccumulatorPairStack, NNUE},
    pretty_print_stats::{pretty_print_stats, print_header},
    repetition_tracker::RepetitionTracker,
    search::stats::SearchStats,
    staged_move_generator::{GetMoveResult, StagedMoveGenerator},
    time_management::{get_cutoff_times, modify_cutoff_time},
    transposition_table::{self, MoveType, TTEntry, TranspositionTable},
    uci::UciInterface,
    uci_required_options_helper::RequiredUciOptions,
};

pub static IS_SEARCHING: AtomicBool = AtomicBool::new(false);

/// Each value is in addition to the last
static ASPIRATION_WINDOW_OFFSETS: [u16; 4] = [50, 150, 600, u16::MAX];

pub const EMPTY_MOVE: Move = Move { data: 0 };

include!(concat!(env!("OUT_DIR"), "/ln_fixedpoint_128_values.rs"));

#[derive(PartialEq, Eq)]
enum SearchControl {
    Unknown,
    TimeLeft,
    MoveTime,
    Depth,
    Infinite,
    Nodes,
}

#[derive(Clone)]
pub struct SearchResult {
    pub best_move: Move,
    pub score: i16,
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
    pub killers: [Move; 2],
    static_eval: Option<i16>,
}

struct SearchMonitor {}

pub struct Searcher<'a> {
    stats: SearchStats,
    transposition_table: &'a TranspositionTable,
    history_table: &'a mut HistoryTable,
    starting_fullmove: u8,
    hard_cutoff_time: Option<Instant>,
    single_root_move: bool,
    starting_in_check: bool,
    stop_rx: Option<&'a Receiver<()>>,
    stop_received: bool,
    max_nodes: u64,
    hard_max_nodes: bool,
    continuation_histories: &'a mut ContinuationHistoryTables,
    multi_pv: u8,
    root_pvs: VecDeque<PvData>,
    // This field is intentionally unused except when tuning
    #[allow(dead_code)]
    extra_uci_options: RequiredUciOptions,
    root_pv_branch_nodes: u64,
    pv_nodes_fractions: VecDeque<f32>,
    contempt: i16,
    white_started_search: bool,
    ss: Vec<SearchStack>,
    root_killers: [Move; 2],
    repetitions: Box<RepetitionTracker>,
    use_uci_mode: bool,
    correction_histories: &'a mut CorrectionHistoryTables,
    thread_num: u16,
    stop_search: &'a AtomicBool,
    accumulators: AccumulatorPairStack,
}

/// Returns the stats and result from the main thread
pub fn search_multithreaded<'a, F>(
    threads: u16,
    transposition_table: &'a TranspositionTable,
    thread_histories: &'a mut [ThreadHistoryTables],
    stop_rx: &'a Receiver<()>,
    multi_pv: u8,
    extra_uci_options: RequiredUciOptions,
    contempt: i16,
    repetitions: Box<RepetitionTracker>,
    use_uci_mode: bool,
    board: Board,
    time_options: &Option<UciTimeControl>,
    search_options: &Option<UciSearchControl>,
    on_search_finished: F,
    hard_max_nodes: bool,
    move_overhead: u16,
) -> (SearchResult, SearchStats)
where
    F: Fn(&SearchResult),
{
    assert_ne!(threads, 0);
    assert_eq!(threads as usize, thread_histories.len());

    // Will unset IS_SEARCHING when dropped (when this method returns).
    // I am assuming for now that this method will only be called once at a time in this process.
    let _monitor = SearchMonitor {};
    IS_SEARCHING.store(true, std::sync::atomic::Ordering::Release);

    let (main_history, mut thread_histories) = thread_histories.split_at_mut(1);
    let main_history = &mut main_history[0];
    let stats = SearchStats::default();
    let stop_search = AtomicBool::new(false);
    thread::scope(|s| {
        for thread_num in 1..threads {
            let mut board = board.clone();
            let repetitions = repetitions.clone();
            let extra_uci_options = extra_uci_options.clone();
            // This is some trickery from https://stackoverflow.com/a/58459786.
            // Without this it would move stop_received into the thread and then I can't use it for the main thread.
            // It wouldn't be needed if the closure didn't use the move keyword,
            // but then I can't reference thread_num because it wants to borrow it.
            let stop_search = &stop_search;
            let stats = stats.clone();
            let (thread_history, remaining_history) = thread_histories.split_at_mut(1);
            thread_histories = remaining_history;
            let thread_history = &mut thread_history[0];

            s.spawn(move || {
                let mut searcher = Searcher::new(
                    transposition_table,
                    thread_history,
                    None,
                    multi_pv,
                    extra_uci_options,
                    contempt,
                    repetitions,
                    use_uci_mode,
                    thread_num,
                    stop_search,
                    stats,
                    hard_max_nodes,
                );

                searcher.initialize_with_board(&board);

                // This is a shortened version of iterative_deepening_search that just searches until the main thread says to stop
                let mut last_result = None;
                for draft in 1..=255 {
                    let single_move = searcher.alpha_beta_init(&mut board, draft, last_result);

                    if single_move {
                        break;
                    }

                    if let Some(best_pv) = searcher.root_pvs.front() {
                        if stop_search.load(std::sync::atomic::Ordering::Relaxed) {
                            break;
                        }

                        last_result = Some(best_pv.search_result.clone());
                    } else {
                        // Search must have been stopped
                        break;
                    }
                }
            });
        }

        let searcher = Searcher::new(
            transposition_table,
            main_history,
            Some(stop_rx),
            multi_pv,
            extra_uci_options,
            contempt,
            repetitions,
            use_uci_mode,
            0,
            &stop_search,
            stats,
            hard_max_nodes,
        );
        let result = searcher.iterative_deepening_search(board, time_options, search_options, move_overhead);

        // Ensure the other threads know to stop whenever the main thread stops
        stop_search.store(true, std::sync::atomic::Ordering::Relaxed);

        on_search_finished(&result.0);

        result
    })
}

impl<'a> Searcher<'a> {
    fn new(
        transposition_table: &'a TranspositionTable,
        thread_histories: &'a mut ThreadHistoryTables,
        stop_rx: Option<&'a Receiver<()>>,
        multi_pv: u8,
        extra_uci_options: RequiredUciOptions,
        contempt: i16,
        repetitions: Box<RepetitionTracker>,
        use_uci_mode: bool,
        thread_num: u16,
        stop_search: &'a AtomicBool,
        stats: SearchStats,
        hard_max_nodes: bool,
    ) -> Self {
        assert!(multi_pv >= 1);

        Self {
            stats,
            transposition_table,
            history_table: &mut thread_histories.history_table,
            starting_fullmove: 0xFF,
            hard_cutoff_time: None,
            single_root_move: false,
            starting_in_check: true,
            stop_rx,
            stop_received: false,
            max_nodes: u64::MAX,
            hard_max_nodes,
            continuation_histories: &mut thread_histories.continuation_histories,
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
            use_uci_mode,
            correction_histories: &mut thread_histories.correction_histories,
            thread_num,
            stop_search,
            accumulators: AccumulatorPairStack::new(),
        }
    }

    /// These values need to be set no matter how the search is started
    fn initialize_with_board(&mut self, board: &Board) {
        self.starting_fullmove = board.fullmove_counter as u8;
        self.starting_in_check = board.is_in_check(false);
        self.white_started_search = board.white_to_move;
        self.accumulators.init(board, &NNUE);
    }

    fn iterative_deepening_search(
        mut self,
        mut board: Board,
        time_options: &Option<UciTimeControl>,
        search_options: &Option<UciSearchControl>,
        move_overhead: u16,
    ) -> (SearchResult, SearchStats) {
        let start_time = Instant::now();
        let mut soft_cutoff_time = None;
        let mut search_control: SearchControl;
        let mut max_depth = 40;
        let mut cutoff_times = None;

        if !self.use_uci_mode {
            print_header();
        }

        self.initialize_with_board(&board);

        self.hard_cutoff_time = None;
        if let Some(t) = time_options {
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
                    self.hard_cutoff_time = Some(
                        cutoff_times
                            .as_ref()
                            .unwrap()
                            .hard_cutoff
                            .checked_sub(Duration::from_millis(move_overhead as u64))
                            .unwrap(),
                    );

                    search_control = SearchControl::TimeLeft;
                }
                UciTimeControl::MoveTime(time_delta) => {
                    let target_dur = time_delta.to_std().unwrap();
                    soft_cutoff_time = Some(target_dur);
                    self.hard_cutoff_time = Some(start_time.checked_add(target_dur).unwrap());
                    search_control = SearchControl::MoveTime;
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

        if let Some(search) = search_options {
            if let Some(depth) = search.depth {
                if search_control == SearchControl::Unknown {
                    search_control = SearchControl::Depth;
                }
                max_depth = depth;
            } else if let Some(nodes) = search.nodes {
                if search_control == SearchControl::Unknown {
                    search_control = SearchControl::Nodes;
                }
                self.max_nodes = nodes;
            }
        }

        if search_control == SearchControl::Unknown {
            error!("Please specify wtime and btime, infinite, depth, or nodes when calling go.");
            panic!("Please specify wtime and btime, infinite, depth, or nodes when calling go.");
        }

        let mut latest_result = None;
        for depth in 1..=255 {
            let end_search = self.alpha_beta_init(&mut board, depth, latest_result.clone());
            if let Some(best_pv) = self.root_pvs.front()
                && let Some(worst_pv) = self.root_pvs.back()
            {
                let elapsed = start_time.elapsed();

                for (i, pv) in self.root_pvs.iter().enumerate() {
                    if self.use_uci_mode {
                        UciInterface::print_search_info(
                            pv.search_result.score,
                            &self.stats,
                            &elapsed,
                            self.transposition_table,
                            &pv.pv,
                            self.starting_fullmove,
                            i as u8 + 1,
                            pv.selective_depth,
                        );
                    } else {
                        pretty_print_stats(
                            pv.search_result.score,
                            &self.stats,
                            &elapsed,
                            self.transposition_table,
                            &pv.pv,
                            self.starting_fullmove,
                            i as u8 + 1,
                            pv.selective_depth,
                            &board,
                        );
                    }
                }

                self.stats.aspiration_researches = 0;

                if self.stop_received
                    || (search_control != SearchControl::Infinite
                        && (end_search
                            || worst_pv.search_result.score.abs() >= MATE_THRESHOLD
                            || depth >= max_depth
                            || self.stats.global_total_nodes() >= self.max_nodes))
                {
                    return (best_pv.search_result.clone(), self.stats);
                }

                if search_control == SearchControl::TimeLeft || search_control == SearchControl::MoveTime {
                    if search_control == SearchControl::TimeLeft
                        && let Some(cutoff_times) = &cutoff_times
                    {
                        // Recreate behavior from when dropping into qsearch counted as 2 nodes. total_search_leaves is added into root_pv_branch_nodes in search.
                        let pv_nodes_fraction = self.root_pv_branch_nodes as f32
                            / (self.stats.thread_total_nodes() - self.stats.start_of_iteration_nodes
                                + self.stats.total_search_leaves
                                - self.stats.start_of_iteration_search_leaves) as f32;

                        if self.pv_nodes_fractions.len() >= 3 {
                            self.pv_nodes_fractions.pop_back();
                        }

                        self.pv_nodes_fractions.push_front(pv_nodes_fraction);

                        soft_cutoff_time = Some(modify_cutoff_time(cutoff_times, &self.pv_nodes_fractions));
                    }

                    if elapsed
                        >= soft_cutoff_time
                            .unwrap()
                            .saturating_sub(Duration::from_millis(move_overhead as u64))
                    {
                        return (best_pv.search_result.clone(), self.stats);
                    }
                }

                latest_result = Some(best_pv.search_result.clone());
            } else {
                println!(
                    "info string search interrupted with nodes {}",
                    self.stats.global_total_nodes()
                );

                if !self.stop_received {
                    debug!("Cancelled search of depth {depth} due to exceeding time budget or max nodes being reached");
                }

                return (latest_result
                    .expect("iterative_deepening_search exceeded cancel_search_time or max nodes before completing any searches"), self.stats);
            }
        }

        (latest_result.unwrap(), self.stats)
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
            alpha = last_score.saturating_sub_unsigned(ASPIRATION_WINDOW_OFFSETS[0]);
            beta = last_score.saturating_add_unsigned(ASPIRATION_WINDOW_OFFSETS[0]);
            alpha_window_index = 1;
            beta_window_index = 1;
        }

        let mut pv = tiny_vec!();

        loop {
            self.ss.clear();
            self.root_pvs.clear();
            self.stats.start_of_iteration_nodes = self.stats.thread_total_nodes();
            self.stats.start_of_iteration_search_leaves = self.stats.total_search_leaves;

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
                        .saturating_sub_unsigned(ASPIRATION_WINDOW_OFFSETS[alpha_window_index])
                        .min(-i16::MAX);
                    alpha_window_index += 1;
                }
            } else if high_score >= beta {
                self.stats.aspiration_researches += 1;

                while high_score >= beta {
                    beta = beta.saturating_add_unsigned(ASPIRATION_WINDOW_OFFSETS[beta_window_index]);
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
        mut beta: i16,
        mut draft: u8,
        ply: u8,
        in_check: bool,
        can_null_move: bool,
        parent_pv: &mut TinyVec<[Move; 32]>,
    ) -> Result<i16, ()> {
        debug_assert!(alpha <= beta);

        if ply != 0
            && (board.halfmove_clock >= 100
                || self.repetitions.test_repetition(board)
                || board.is_insufficient_material())
        {
            if self.inc_and_check_thread_nodes() {
                return Err(());
            }

            parent_pv.clear();
            return Ok(self.eval_draw(board));
        }

        self.stats.selective_depth = self.stats.selective_depth.max(ply);

        if ply == 255 {
            if self.inc_and_check_thread_nodes() {
                return Err(());
            }

            return Ok(if in_check {
                self.eval_draw(board)
            } else {
                self.evaluate_nnue(board) + self.correction_histories.get_adjustment(board, &self.ss, ply)
            });
        }

        if in_check {
            draft = draft.saturating_add(1);
        }

        if draft == 0 {
            self.stats.total_search_leaves += 1;

            if self.stats.total_search_leaves % 16384 == 16383 {
                if self.thread_num == 0 {
                    let stop_received = self.stop_rx.is_some_and(|stop_rx| matches!(stop_rx.try_recv(), Ok(())));
                    let global_total_nodes = self.stats.global_total_nodes();
                    if stop_received
                        || self.hard_cutoff_time.is_some_and(|t| Instant::now() >= t)
                        || (self.hard_max_nodes && global_total_nodes >= self.max_nodes)
                        || global_total_nodes >= self.max_nodes.saturating_mul(20)
                    {
                        self.stop_search.store(true, std::sync::atomic::Ordering::Release);
                        if stop_received {
                            self.stop_received = stop_received;
                        }

                        // Nodes hasn't been incremented yet. Not incrementing would indicate the last node to not be searched, which it has been.
                        self.stats.inc_nodes();

                        return Err(());
                    }
                } else if self.stop_search.load(std::sync::atomic::Ordering::Relaxed) {
                    return Err(());
                }
            }

            parent_pv.clear();

            return self.quiescense_side_to_move_relative(board, alpha, beta, ply);
        }

        if self.inc_and_check_thread_nodes() {
            return Err(());
        }

        if ply != 0 {
            // Mate distance pruning
            alpha = alpha.max(-MATE_VALUE + ply as i16);
            beta = beta.min(MATE_VALUE - ply as i16);

            if alpha >= beta {
                return Ok(alpha);
            }
        }

        if self.ss.get(ply as usize).is_none() {
            self.ss.push(SearchStack::new());
        } else {
            self.ss[ply as usize].killers = [EMPTY_MOVE; 2];
            self.ss[ply as usize].static_eval = None;
        }

        let is_pv = alpha + 1 != beta;

        let mut move_gen = StagedMoveGenerator::new();
        let excluded_move = self.ss[ply as usize].excluded_move;
        let tt_entry = if excluded_move.is_none() {
            self.transposition_table.get_entry(board.hash, self.starting_fullmove)
        } else {
            None
        };
        if let Some(tt_data) = tt_entry {
            if !is_pv && tt_data.draft >= draft {
                let tt_score = tt_data.get_score(ply);

                match MoveType::from(tt_data.get_move_type()) {
                    transposition_table::MoveType::FailHigh => {
                        if tt_score >= beta {
                            // should history be updated here?
                            update_killers_and_histories(
                                board,
                                tt_data.important_move,
                                draft,
                                ply,
                                self.history_table,
                                self.continuation_histories,
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

        let mut static_eval = None;
        let mut futility_prune = false;
        if !in_check && excluded_move.is_none() {
            let eval = self.evaluate_nnue(board) + self.correction_histories.get_adjustment(board, &self.ss, ply);
            static_eval = Some(eval);
            self.ss[ply as usize].static_eval = Some(eval);

            if draft < 6 && !is_pv && alpha.abs() < 2000 && beta.abs() < 2000 {
                let improving = if ply > 1
                    && let Some(old_eval) = self.ss[(ply - 2) as usize].static_eval
                {
                    eval > old_eval
                } else if ply > 3
                    && let Some(old_eval) = self.ss[(ply - 4) as usize].static_eval
                {
                    eval > old_eval
                } else {
                    false
                };

                let rfp_threshold = beta + 90 * (draft as i16 - if improving { 1 } else { 0 });
                let rfp_eval = if tt_entry.is_some_and(|e| {
                    e.get_move_type() == MoveType::Best as u8
                        || (e.get_move_type() == MoveType::FailHigh as u8 && e.get_score(ply) >= rfp_threshold)
                        || (e.get_move_type() == MoveType::FailLow as u8 && e.get_score(ply) < rfp_threshold)
                }) {
                    tt_entry.unwrap().get_score(ply)
                } else {
                    eval
                };

                // Reverse futility pruning
                if rfp_eval >= rfp_threshold {
                    return Ok((rfp_eval + beta) / 2);
                }

                if draft < 4 {
                    futility_prune = (eval + 307 + 173 * (draft - 1) as i16) < alpha;

                    // Razoring
                    if (eval + 332 + 279 * (draft - 1) as i16) < alpha {
                        let score = self.quiescense_side_to_move_relative(board, alpha, beta, ply)?;
                        if score < alpha {
                            return Ok(score);
                        }
                    }
                }
            }
        }

        let mut pv: TinyVec<[Move; 32]> = tiny_vec!();

        // Null move pruning
        let our_side = if board.white_to_move { 0 } else { 1 };
        if can_null_move
            && !is_pv
            && beta < i16::MAX
            && draft > 4
            && !in_check
            && excluded_move.is_none()
            && tt_entry.is_none_or(|e| e.get_move_type() != MoveType::FailLow as u8 && e.get_score(ply) >= beta)
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

        // When at the root and with multi-pv enabled then this will be the lowest PV score
        let mut best_score = i16::MIN;
        // When at the root and with multi-pv enabled then this may not be the actual best move
        let mut best_move = None;
        let mut searched_moves = 0;
        let mut has_legal_move = false;
        let mut improved_alpha = false;

        let mut searched_quiet_moves: TinyVec<[Move; 64]> = tiny_vec!();
        let see_margin = (draft.min(181) as i16).pow(2).saturating_mul(-16);
        loop {
            let mov = match move_gen.get_next_move() {
                GetMoveResult::Move(scored_move) => scored_move,
                GetMoveResult::GenerateMoves => {
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

            if excluded_move.is_some_and(|em| em == mov.m) {
                continue;
            }

            // Futility pruning and late move pruning
            if (futility_prune || (!is_pv && !in_check && searched_moves >= 6 && mov.score < 51 + (-92 * draft as i16)))
                && searched_moves >= 1
            {
                move_gen.prune_quiet_non_promos();

                if !mov.is_capture_or_promo() {
                    continue;
                }
            }

            if !is_pv && mov.is_capture() && !board.is_static_exchange_eval_at_least(mov.m, see_margin) {
                continue;
            }

            // test for SE before making move so the accumulator updates from make_move don't get overwitten
            let mut extension = 0;
            if ply != 0
                && draft >= 5
                && excluded_move.is_none()
                && tt_entry.is_some_and(|tt_entry| {
                    tt_entry.important_move == mov.m
                        && tt_entry.draft >= draft - 3
                        && tt_entry.get_move_type() != MoveType::FailLow as u8
                        && tt_entry.get_score(ply).abs() < MATE_THRESHOLD - 2600
                })
            {
                self.ss[ply as usize].excluded_move = Some(mov.m);

                let verification_draft = draft / 2;
                let verification_beta = tt_entry.unwrap().get_score(ply) - draft as i16 * 2;
                let beginning_nodes = self.stats.thread_total_nodes();

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
                if self.stats.thread_total_nodes() == beginning_nodes + 1 {
                    extension = 1;
                } else if verification_score < verification_beta {
                    extension = 1;
                }

                self.ss[ply as usize].excluded_move = None;
            }

            let mut new_board = board.clone();
            let (legal, move_made) = new_board.test_legality_and_maybe_make_move(
                mov.m,
                &mut self.repetitions,
                Some(&mut self.accumulators),
                Some(&NNUE),
            );
            if !legal {
                if move_made {
                    self.unmake_move(&new_board);
                }
                continue;
            }

            has_legal_move = true;

            self.ss[ply as usize].mov = mov.m;
            self.ss[ply as usize].moved_piece_type = new_board.get_piece_64(mov.m.to() as usize) & PIECE_MASK;

            let gives_check = new_board.is_in_check(false);
            let start_of_search_nodes = if ply == 0 {
                self.stats.thread_total_nodes() + self.stats.total_search_leaves
            } else {
                0
            };
            if ply == 0 && self.multi_pv != 1 {
                self.stats.selective_depth = 0;
            }

            let mut score;
            if searched_moves == 0 {
                // Full search
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
            } else {
                // Late move reduction
                let reduction_ply = if draft > 2 && searched_moves > 3 {
                    // Using formula and values from Ethereal according to https://www.chessprogramming.org/Late_Move_Reductions
                    let mut reduction_fixedpoint_128 = if !mov.is_capture_or_promo() {
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

                // Use zero window and reduction
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

                if score > alpha && reduction_ply > 0 {
                    // Do a full depth zero window search
                    score = -self.alpha_beta_recurse(
                        &mut new_board,
                        -alpha - 1,
                        -alpha,
                        draft - 1 + extension,
                        ply + 1,
                        gives_check,
                        can_null_move,
                        &mut pv,
                    )?;
                }

                if score > alpha && score < beta {
                    // Do a full search, this is a pv candidate
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

            self.unmake_move(&new_board);
            searched_moves += 1;

            if score >= beta {
                let mut relevant_cont_histories = update_killers_and_histories(
                    board,
                    mov.m,
                    draft,
                    ply,
                    self.history_table,
                    self.continuation_histories,
                    &mut self.ss,
                    &mut self.root_killers,
                );

                let penalty = -(draft as i16) * (draft as i16);
                for sqm in searched_quiet_moves {
                    update_histories(board, self.history_table, sqm, penalty, &mut relevant_cont_histories);
                }

                if ply == 0 {
                    let pv_data = PvData {
                        pv: TinyVec::default(),
                        search_result: SearchResult {
                            best_move: mov.m,
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
                        mov.m,
                        MoveType::FailHigh,
                        score,
                        draft,
                        ply,
                        self.starting_fullmove,
                    ));

                    // Currently static eval is always set when not in check
                    if let Some(static_eval) = static_eval
                        && score >= static_eval
                        && !mov.is_capture()
                    {
                        self.correction_histories.update_history(
                            board,
                            score.saturating_sub(static_eval),
                            draft,
                            &self.ss,
                            ply,
                        );
                    }
                }

                return Ok(score);
            }

            if score > best_score {
                if ply == 0 {
                    *parent_pv = pv.clone();
                    parent_pv.push(mov.m);

                    let pv_data = PvData {
                        pv: parent_pv.clone(),
                        search_result: SearchResult {
                            best_move: mov.m,
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
                        self.root_pv_branch_nodes =
                            self.stats.thread_total_nodes() + self.stats.total_search_leaves - start_of_search_nodes;
                    }

                    score = self.root_pvs.back().unwrap().search_result.score;
                }

                if ply != 0 || self.root_pvs.len() == self.multi_pv as usize {
                    best_score = score;
                    best_move = Some(mov.m);
                    if score > alpha {
                        alpha = score;
                        improved_alpha = true;

                        // This will be handled separately for root nodes
                        if is_pv && ply != 0 {
                            *parent_pv = pv.clone();
                            parent_pv.push(mov.m);
                        }
                    }
                }
            }

            // TODO: change to check for capture flag
            if mov.m.flags() == 0 {
                searched_quiet_moves.push(mov.m);
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
            let best_move = best_move.unwrap();

            let entry_type = if improved_alpha {
                MoveType::Best
            } else {
                MoveType::FailLow
            };
            self.transposition_table.store_entry(TTEntry::new(
                board.hash,
                best_move,
                entry_type,
                best_score,
                draft,
                ply,
                self.starting_fullmove,
            ));

            // Currently static eval is always set when not in check
            if let Some(static_eval) = static_eval
                && (improved_alpha || best_score <= static_eval)
                && !best_move.is_capture()
            {
                self.correction_histories.update_history(
                    board,
                    best_score.saturating_sub(static_eval),
                    draft,
                    &self.ss,
                    ply,
                );
            }
        }

        Ok(best_score)
    }

    pub fn quiescense_side_to_move_relative(
        &mut self,
        board: &mut Board,
        mut alpha: i16,
        beta: i16,
        ply: u8,
    ) -> Result<i16, ()> {
        if self.inc_and_check_thread_nodes() {
            return Err(());
        }

        if board.is_insufficient_material() {
            return Ok(self.eval_draw(board));
        }

        let is_pv = alpha + 1 != beta;
        let mut moves = ArrayVec::new();
        let tt_entry = self.transposition_table.get_entry(board.hash, self.starting_fullmove);
        if let Some(tt_data) = tt_entry {
            let tt_eval = tt_data.get_score(ply);

            if !is_pv {
                match MoveType::from(tt_data.get_move_type()) {
                    transposition_table::MoveType::FailHigh => {
                        if tt_eval >= beta {
                            return Ok(tt_eval);
                        }
                    }
                    transposition_table::MoveType::Best => {
                        return Ok(tt_eval);
                    }
                    transposition_table::MoveType::FailLow => {
                        if tt_eval < alpha {
                            return Ok(tt_eval);
                        }
                    }
                }
            }

            if tt_data.important_move.is_capture() {
                moves.push(ScoredMove {
                    m: tt_data.important_move,
                    score: 1,
                });
            }
        }

        // static eval is not currently tracked in search stack in qsearch
        if self.ss.get(ply as usize).is_none() {
            self.ss.push(SearchStack::new());
        }

        let mut best_score;
        let in_check = board.is_in_check(false);

        // Need to not do anything that would go to the next ply before this
        if ply == 255 {
            return Ok(if in_check {
                self.eval_draw(board)
            } else {
                self.evaluate_nnue(board) + self.correction_histories.get_adjustment(board, &self.ss, ply)
            });
        }

        if !in_check {
            let stand_pat = self.evaluate_nnue(board) + self.correction_histories.get_adjustment(board, &self.ss, ply);

            if stand_pat >= beta {
                return Ok(stand_pat);
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
                let mov = moves[move_index];

                if round == 1 && tt_entry.is_some_and(|v| v.important_move == mov.m) {
                    continue;
                }

                // SEE is coded to be run before the move is made so have to do it before testing legality
                if !board.is_static_exchange_eval_at_least(mov.m, 0) {
                    continue;
                }

                let mut new_board = board.clone();
                let (legal, move_made) = new_board.test_legality_and_maybe_make_move(
                    mov.m,
                    &mut self.repetitions,
                    Some(&mut self.accumulators),
                    Some(&NNUE),
                );
                if !legal {
                    if move_made {
                        self.unmake_move(&new_board);
                    }
                    continue;
                }

                self.ss[ply as usize].mov = mov.m;
                self.ss[ply as usize].moved_piece_type = new_board.get_piece_64(mov.m.to() as usize) & PIECE_MASK;

                // Only doing captures right now so not checking halfmove or threefold repetition here
                let score = -self.quiescense_side_to_move_relative(&mut new_board, -beta, -alpha, ply + 1)?;

                self.unmake_move(&new_board);

                if score >= beta {
                    self.transposition_table.store_entry(TTEntry::new(
                        board.hash,
                        mov.m,
                        MoveType::FailHigh,
                        score,
                        0,
                        0,
                        self.starting_fullmove,
                    ));

                    return Ok(score);
                }

                if best_score < score {
                    best_score = score;
                    best_move = Some(mov.m);

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

        Ok(best_score)
    }

    fn eval_draw(&self, board: &Board) -> i16 {
        self.contempt
            * if board.white_to_move ^ self.white_started_search {
                1
            } else {
                -1
            }
    }

    #[must_use]
    /// Returns true if max nodes exceeded
    fn inc_and_check_thread_nodes(&mut self) -> bool {
        self.stats.inc_nodes();

        self.hard_max_nodes && self.stats.thread_total_nodes() >= self.max_nodes
    }

    fn evaluate_nnue(&self, board: &Board) -> i16 {
        let pair = self.accumulators.get_current_accumulator();
        if board.white_to_move {
            NNUE.evaluate(&pair.white, &pair.black)
        } else {
            NNUE.evaluate(&pair.black, &pair.white)
        }
    }

    /// Does not modify the board, but does update the repetition tracker and change which accumulator is current
    fn unmake_move(&mut self, new_board: &Board) {
        self.repetitions.unmake_move(new_board.hash);
        self.accumulators.decr_ply();
    }
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
            static_eval: None,
        }
    }

    pub fn get_mov(&self) -> Move {
        self.mov
    }

    pub fn get_moved_piece_type(&self) -> u8 {
        self.moved_piece_type
    }
}

impl Drop for SearchMonitor {
    fn drop(&mut self) {
        IS_SEARCHING.store(false, std::sync::atomic::Ordering::Release);
    }
}

pub mod stats {
    use std::sync::{Arc, atomic::AtomicU64};

    #[derive(Default, Clone)]
    pub struct SearchStats {
        pub depth: u8,
        thread_total_nodes: u64,
        global_total_nodes: Arc<AtomicU64>,
        pub start_of_iteration_nodes: u64,
        pub start_of_iteration_search_leaves: u64,
        pub total_search_leaves: u64,
        pub aspiration_researches: u8,
        pub selective_depth: u8,
    }

    impl SearchStats {
        pub fn thread_total_nodes(&self) -> u64 {
            self.thread_total_nodes
        }

        /// Warning: atomic load
        pub fn global_total_nodes(&self) -> u64 {
            self.global_total_nodes.load(std::sync::atomic::Ordering::Relaxed)
        }

        pub fn inc_nodes(&mut self) {
            self.thread_total_nodes += 1;
            self.global_total_nodes
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
    }
}
