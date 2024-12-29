use std::{process::exit, time::{Duration, Instant}};

use build_info::build_info;
use build_info::VersionControl::Git;
use log::{debug, error, trace};
use vampirc_uci::{parse_with_unknown, UciMessage, UciPiece};

use crate::{
    board::Board, moves::{find_and_run_moves, FLAGS_PROMO_BISHOP, FLAGS_PROMO_KNIGHT, FLAGS_PROMO_QUEEN, FLAGS_PROMO_ROOK}, search::SearchStats, STARTING_FEN
};

#[derive(Default)]
pub struct UciInterface {
    board: Option<Board>,
}

build_info!(fn get_build_info);

impl UciInterface {
    // how to communicate with the engine while it is computing? How necessary is that?
    pub fn process_command(&mut self, cmd: String) {
        debug!("Received UCI cmd string '{cmd}'");
        let messages = parse_with_unknown(&cmd);
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
                }
                UciMessage::IsReady => {
                    println!("readyok")
                }
                UciMessage::UciNewGame => {
                    self.board = None;
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
                    if let Some(b) = self.board.as_mut() {
                        let move_data = b.iterative_deepening_search(&time_control, &search_control);

                        println!("bestmove {}", move_data.0.simple_long_algebraic_notation())
                    }
                }
                UciMessage::Stop => {
                    // error!("UCI stop command but this is not implemented");
                    // unimplemented!("UCI stop command")
                    // println!("bestmove <>")
                }
                UciMessage::Quit => exit(0),
                UciMessage::Unknown(message, err) => {
                    error!("Unknown UCI cmd in '{message}'. Parsing error: {err:?}")
                }
                _ => {
                    error!("Unhandled UCI cmd in '{cmd}'")
                }
            }
        }
    }

    pub fn print_search_info(eval_opt: Option<i32>, stats: &SearchStats, elapsed: &Duration) {
        let score_string;
        if let Some(eval) = eval_opt {
            let abs_cp = eval.abs();
            if abs_cp >= 19800 {
                let diff = 20000 - abs_cp;
                let moves = (diff as f32 / 20.0).ceil();
                score_string = format!("score mate {}{moves}", if eval < 0 { "-" } else { "" });
            } else {
                score_string = format!("score cp {eval}");
            }
        } else {
            score_string = "".to_string();
        }

        let nps = stats.nodes as f64 / elapsed.as_secs_f64();
        println!(
            "info {score_string} nodes {} depth {} nps {:.0} time {}",
            stats.nodes,
            stats.depth,
            nps,
            elapsed.as_millis()
        );
    }
}
