use std::{
    process::exit,
    time::{Duration, Instant},
};

use build_info::VersionControl::Git;
use build_info::build_info;
use log::{debug, error, trace};
use vampirc_uci::{UciMessage, UciPiece, parse_with_unknown};

use crate::{
    STARTING_FEN,
    board::Board,
    evaluate::{MATE_THRESHOLD, MATE_VALUE},
    moves::{FLAGS_PROMO_BISHOP, FLAGS_PROMO_KNIGHT, FLAGS_PROMO_QUEEN, FLAGS_PROMO_ROOK, find_and_run_moves},
    search::{HistoryTable, SearchStats, Searcher},
    transposition_table::TranspositionTable,
};

pub struct UciInterface {
    board: Option<Board>,
    transposition_table: TranspositionTable,
    history_table: HistoryTable,
}

build_info!(fn get_build_info);

impl UciInterface {
    pub fn new(tt_size_log_2: u8) -> UciInterface {
        UciInterface {
            board: None,
            transposition_table: TranspositionTable::new(tt_size_log_2),
            history_table: [[[0; 64]; 6]; 2],
        }
    }

    // how to communicate with the engine while it is computing? How necessary is that?
    pub fn process_command(&mut self, cmd: &str) {
        debug!("Received UCI cmd string '{cmd}'");
        let messages = parse_with_unknown(cmd);
        for m in messages {
            match m {
                UciMessage::Uci => {
                    let build_info = get_build_info();
                    let commit;
                    match &build_info.version_control {
                        Some(vc) => match vc {
                            Git(g) => {
                                commit = format!("{}{}", g.commit_short_id, if g.dirty { "*" } else { "" });
                            }
                        },
                        None => {
                            commit = "".to_string();
                        }
                    }

                    println!("id name FoxChess {} {}", build_info.profile, commit);
                    println!("id author IDK");
                    println!("uciok");
                    // println!("option name IsolatedPawnPenalty type spin default 35 min -100 max 100");
                    // println!("option name DoubledPawnPenalty type spin default 25 min 0 max 100");
                }
                UciMessage::IsReady => {
                    println!("readyok")
                }
                UciMessage::UciNewGame => {
                    self.board = None;
                    self.transposition_table.clear();
                    self.history_table = [[[0; 64]; 6]; 2];
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
                        debug!("running {} moves", moves.len());
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
                    debug!("Position with {} moves took {duration:#?} to calculate", moves.len());

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
                        let mut searcher =
                            Searcher::new(&mut board_copy, &mut self.transposition_table, &mut self.history_table);

                        let search_result = searcher.iterative_deepening_search(&time_control, &search_control);

                        println!("bestmove {}", search_result.best_move.simple_long_algebraic_notation());

                        debug!(
                            "transposition_table index collisions {}",
                            self.transposition_table.index_collisions
                        );
                        self.transposition_table.index_collisions = 0;
                    } else {
                        error!("Board must be set with position first");
                    }
                }
                UciMessage::Stop => {
                    // error!("UCI stop command but this is not implemented");
                    // unimplemented!("UCI stop command")
                    // println!("bestmove <>")
                }
                UciMessage::Quit => exit(0),
                UciMessage::SetOption { name, .. } => match name.as_str() {
                    // "IsolatedPawnPenalty" => {
                    //     if let Some(ipp) = value {
                    //         ISOLATED_PAWN_PENALTY.set(
                    //             ipp.parse()
                    //                 .expect("IsolatedPawnPenalty setoption value was not a valid number"),
                    //         );
                    //     }
                    // }
                    // "DoubledPawnPenalty" => {
                    //     if let Some(ipp) = value {
                    //         DOUBLED_PAWN_PENALTY.set(
                    //             ipp.parse()
                    //                 .expect("DoubledPawnPenalty setoption value was not a valid number"),
                    //         );
                    //     }
                    // }
                    _ => {
                        error!("Unknown UCI setoption name '{name}'");
                    }
                },
                UciMessage::Unknown(message, err) => {
                    if message.starts_with("go perft") {
                        let parts = message.split(' ').collect::<Vec<_>>();
                        if parts.len() < 3 {
                            error!("Expected format: go perft [depth]");
                            return;
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
                    } else {
                        error!("Unknown UCI cmd in '{message}'. Parsing error: {err:?}");
                    }
                }
                _ => {
                    error!("Unhandled UCI cmd in '{cmd}'");
                }
            }
        }
    }

    pub fn print_search_info(eval: i16, stats: &SearchStats, elapsed: &Duration) {
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
            "info {score_string} nodes {} depth {} nps {:.0} time {} pv {} str aspiration_researches {}",
            stats.total_nodes,
            stats.depth,
            nps,
            elapsed.as_millis(),
            stats
                .pv
                .iter()
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
}
