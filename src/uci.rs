use std::{
    alloc::{Layout, alloc_zeroed},
    io,
    sync::mpsc::{self, Receiver},
    thread,
    time::{Duration, Instant},
};

use build_info::VersionControl::Git;
use log::{debug, error, trace};
use tinyvec::TinyVec;
use vampirc_uci::{UciMessage, UciPiece, parse_with_unknown};

use crate::{
    STARTING_FEN,
    bench::bench,
    board::Board,
    evaluate::{MATE_THRESHOLD, MATE_VALUE},
    get_build_info,
    moves::{FLAGS_PROMO_BISHOP, FLAGS_PROMO_KNIGHT, FLAGS_PROMO_QUEEN, FLAGS_PROMO_ROOK, Move, find_and_run_moves},
    search::{ContinuationHistoryTable, HistoryTable, SearchStats, Searcher},
    transposition_table::{TTEntry, TranspositionTable},
};

pub struct UciInterface {
    board: Option<Board>,
    transposition_table: TranspositionTable,
    history_table: HistoryTable,
    stop_rx: Receiver<()>,
    continuation_history: Box<ContinuationHistoryTable>,
    multi_pv: u8,
}

impl UciInterface {
    pub fn new(tt_size_log_2: u8, stop_rx: Receiver<()>) -> UciInterface {
        UciInterface {
            board: None,
            transposition_table: TranspositionTable::new(tt_size_log_2),
            history_table: [[[0; 64]; 6]; 2],
            stop_rx,
            continuation_history: Self::alloc_zeroed_continuation_history(),
            multi_pv: 1,
        }
    }

    // how to communicate with the engine while it is computing? How necessary is that?
    pub fn process_command(&mut self, cmds: (String, Vec<UciMessage>)) -> bool {
        debug!("Received UCI cmd string (trimmed) '{}'", cmds.0.trim());
        for m in cmds.1 {
            match m {
                UciMessage::Uci => {
                    println!("id name FoxChess {}", UciInterface::get_version());
                    println!("id author nfaltermeier");
                    println!("option name Hash type spin default 128 min 1 max 1048576");
                    println!("option name MultiPV type spin default 1 min 1 max 255");
                    println!("uciok");
                }
                UciMessage::IsReady => {
                    println!("readyok")
                }
                UciMessage::UciNewGame => {
                    self.board = None;
                    self.transposition_table.clear();
                    self.history_table = [[[0; 64]; 6]; 2];
                    self.continuation_history = Self::alloc_zeroed_continuation_history();
                }
                UciMessage::Position { startpos, fen, moves } => {
                    // TODO: optimize for how cutechess works, try to not recalculate the whole game? Or recalculate without searching for moves?
                    let start = Instant::now();
                    if startpos {
                        self.board = Some(Board::from_fen(STARTING_FEN).unwrap())
                    } else if fen.is_some() {
                        let fen_str = fen.unwrap().0;
                        let result = Board::from_fen(&fen_str);
                        match result {
                            Ok(b) => self.board = Some(b),
                            Err(err_msg) => {
                                error!(
                                    "Failed to parse FEN from UCI. Error message: {err_msg}. FEN: {}",
                                    fen_str
                                )
                            }
                        }
                    }

                    if !moves.is_empty() && self.board.is_some() {
                        trace!("running {} moves", moves.len());
                        let mapped = moves.iter().map(|m| {
                            let from = (m.from.file as u8) - b'a' + ((m.from.rank - 1) * 8);
                            let to = (m.to.file as u8) - b'a' + ((m.to.rank - 1) * 8);
                            let mut promo = None;
                            if m.promotion.is_some() {
                                promo = match m.promotion.unwrap() {
                                    UciPiece::Knight => Some(FLAGS_PROMO_KNIGHT),
                                    UciPiece::Bishop => Some(FLAGS_PROMO_BISHOP),
                                    UciPiece::Rook => Some(FLAGS_PROMO_ROOK),
                                    UciPiece::Queen => Some(FLAGS_PROMO_QUEEN),
                                    _ => {
                                        error!("Unexpected promotion value '{:?}'", m.promotion.unwrap());
                                        panic!("Unexpected promotion value")
                                    }
                                };
                            }

                            (from, to, promo)
                        });

                        find_and_run_moves(self.board.as_mut().unwrap(), mapped.collect())
                    }
                    let duration = start.elapsed();
                    trace!("Position with {} moves took {duration:#?} to calculate", moves.len());

                    trace!("At end of position. {:#?}", self.board);
                }
                UciMessage::Go {
                    time_control,
                    search_control,
                } => {
                    trace!("At start of go. {:#?}", self.board);
                    if let Some(b) = &self.board {
                        // Search on a board copy to protect against the board state being changed by the search timing out
                        let mut board_copy = b.clone();
                        let mut searcher = Searcher::new(
                            &mut board_copy,
                            &mut self.transposition_table,
                            &mut self.history_table,
                            &self.stop_rx,
                            &mut self.continuation_history,
                            self.multi_pv,
                        );

                        let search_result = searcher.iterative_deepening_search(&time_control, &search_control);

                        println!("bestmove {}", search_result.best_move.simple_long_algebraic_notation());
                    } else {
                        error!("Board must be set with position first");
                    }
                }
                // Stop is handled with a separate sender and receiver to communicate with a running search so nothing needs to be done here
                UciMessage::Stop => {}
                UciMessage::Quit => {
                    return true;
                }
                UciMessage::SetOption { name, value } => match name.to_ascii_lowercase().as_str() {
                    "hash" => {
                        if let Some(value) = value {
                            let hash_mib = value.parse::<usize>();
                            if let Ok(hash_mib) = hash_mib {
                                let hash_bytes = hash_mib * 1024 * 1024;

                                if hash_bytes == 0 {
                                    error!("Minimum value is 1 (MiB)");
                                    continue;
                                }

                                let entries = hash_bytes / size_of::<TTEntry>();
                                let entries_log2 = entries.checked_ilog2().unwrap();

                                if entries_log2 > u8::MAX as u32 {
                                    error!("Value is too big");
                                    continue;
                                } else if entries_log2 < 2 {
                                    error!("Something went wrong, entries_log2 is less than 2");
                                    continue;
                                }

                                self.transposition_table = TranspositionTable::new(entries_log2 as u8);
                            } else {
                                error!(
                                    "Failed to parse Hash value as a natural number: {}",
                                    hash_mib.unwrap_err()
                                );
                            }
                        } else {
                            error!("Expected a value for option Hash");
                        }
                    }
                    "multipv" => {
                        if let Some(value) = value {
                            let multi_pv = value.parse::<u8>();
                            if let Ok(multi_pv) = multi_pv {
                                if multi_pv == 0 {
                                    error!("MultiPV value must be at least 1");
                                } else {
                                    self.multi_pv = multi_pv;
                                }
                            } else {
                                error!(
                                    "Failed to parse MultiPV value as a natural number: {}",
                                    multi_pv.unwrap_err()
                                );
                            }
                        } else {
                            error!("Expected a value for option MultiPV");
                        }
                    }
                    _ => {
                        error!("Unknown UCI setoption name '{name}'");
                    }
                },
                UciMessage::Unknown(message, err) => {
                    if message.starts_with("go perft") {
                        let parts = message.split(' ').collect::<Vec<_>>();
                        if parts.len() < 3 {
                            error!("Expected format: go perft [depth]");
                            continue;
                        }

                        if let Some(board) = &mut self.board {
                            match parts.get(2).unwrap().parse::<u8>() {
                                Ok(depth) => {
                                    board.start_perft(depth, true);
                                }
                                Err(e) => {
                                    error!("Failed to parse depth argument as u8. Error: {e:#?}");
                                }
                            }
                        } else {
                            error!("Board must be set with position first");
                        }
                    } else if message.starts_with("fen") {
                        if let Some(board) = &self.board {
                            println!("Current fen: {}", board.to_fen())
                        } else {
                            error!("Board must be set with position first");
                        }
                    } else if message.eq_ignore_ascii_case("bench") {
                        bench();
                    } else {
                        error!("Unknown UCI cmd in '{message}'. Parsing error: {err:?}");
                    }
                }
                _ => {
                    error!("Unhandled UCI cmd in (trimmed) '{}'", cmds.0.trim());
                }
            }
        }

        false
    }

    pub fn print_search_info(
        eval: i16,
        stats: &SearchStats,
        elapsed: &Duration,
        transposition_table: &TranspositionTable,
        pv: &TinyVec<[Move; 32]>,
        search_starting_fullmove: u8,
        multi_pv: u8,
    ) {
        let abs_cp = eval.abs();
        let score_string = if abs_cp >= MATE_THRESHOLD {
            let diff = MATE_VALUE - abs_cp;
            let moves = (diff as f32 / 20.0).ceil();
            format!("score mate {}{moves}", if eval < 0 { "-" } else { "" })
        } else {
            format!("score cp {eval}")
        };

        let nps = stats.total_nodes as f64 / elapsed.as_secs_f64();
        println!(
            "info depth {} multipv {multi_pv} {score_string} time {} nodes {} nps {:.0} hashfull {} pv {} string aspiration_researches {}",
            stats.depth,
            elapsed.as_millis(),
            stats.total_nodes,
            nps,
            transposition_table.hashfull(search_starting_fullmove),
            pv.iter()
                .rev()
                .map(|m| m.simple_long_algebraic_notation())
                .collect::<Vec<String>>()
                .join(" "),
            stats.aspiration_researches,
        );
    }

    /// For testing
    pub fn get_board_copy(&self) -> Option<Board> {
        self.board.clone()
    }

    // Based off of https://stackoverflow.com/a/55201400
    pub fn process_stdin_uci() -> (Receiver<(String, Vec<UciMessage>)>, Receiver<()>) {
        let (message_tx, message_rx) = mpsc::channel::<(String, Vec<UciMessage>)>();
        let (stop_tx, stop_rx) = mpsc::channel::<()>();
        thread::spawn(move || {
            loop {
                let mut buffer = String::new();
                io::stdin().read_line(&mut buffer).unwrap();
                let messages = parse_with_unknown(&buffer);

                for m in &messages {
                    match m {
                        UciMessage::Stop | UciMessage::Quit => {
                            stop_tx.send(()).expect("sending stop command failed");
                        }
                        _ => {}
                    }
                }

                message_tx
                    .send((buffer, messages))
                    .expect("sending uci commands failed");
            }
        });
        (message_rx, stop_rx)
    }

    pub fn alloc_zeroed_continuation_history() -> Box<ContinuationHistoryTable> {
        unsafe {
            let mem =
                alloc_zeroed(Layout::array::<i16>(size_of::<ContinuationHistoryTable>() / size_of::<i16>()).unwrap());
            let typed_mem = mem.cast::<ContinuationHistoryTable>();
            Box::from_raw(typed_mem)
        }
    }

    pub fn get_version() -> String {
        let build_info = get_build_info();
        let commit = match &build_info.version_control {
            Some(vc) => match vc {
                Git(g) => {
                    format!(
                        "{}{}",
                        g.tags.first().unwrap_or(&g.commit_short_id),
                        if g.dirty { "*" } else { "" }
                    )
                }
            },
            None => "".to_string(),
        };

        format!("{} {}", build_info.profile, commit)
    }
}
