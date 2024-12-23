use std::process::exit;

use log::{debug, error};
use vampirc_uci::{parse_with_unknown, UciMessage, UciPiece};

use crate::{
    board::Board,
    moves::{
        find_and_run_moves, square_indices_to_moves, MoveRollback, MOVE_PROMO_BISHOP, MOVE_PROMO_KNIGHT,
        MOVE_PROMO_QUEEN, MOVE_PROMO_ROOK,
    },
    STARTING_FEN,
};

#[derive(Default)]
pub struct UciInterface {
    board: Option<Board>,
}

impl UciInterface {
    // how to communicate with the engine while it is computing? How necessary is that?
    pub fn process_command(&mut self, cmd: String) {
        debug!("Received UCI cmd string '{cmd}'");
        let messages = parse_with_unknown(&cmd);
        for m in messages {
            match m {
                UciMessage::Uci => {
                    println!("id name FoxChess");
                    println!("id author IDK");
                    println!("uciok");
                }
                UciMessage::IsReady => {
                    println!("readyok")
                }
                UciMessage::UciNewGame => {
                    // Nothing to do for this for now
                }
                UciMessage::Position { startpos, fen, moves } => {
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
                                    UciPiece::Knight => Some(MOVE_PROMO_KNIGHT),
                                    UciPiece::Bishop => Some(MOVE_PROMO_BISHOP),
                                    UciPiece::Rook => Some(MOVE_PROMO_ROOK),
                                    UciPiece::Queen => Some(MOVE_PROMO_QUEEN),
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

                    debug!("At end of position. {:#?}", self.board);
                }
                UciMessage::Go {
                    time_control,
                    search_control,
                } => {
                    debug!("At start of go. {:#?}", self.board);
                    match self.board.as_mut() {
                        Some(b) => {
                            let r#move = b.search();
                            println!("bestmove {}", r#move.simple_long_algebraic_notation())
                        }
                        None => {}
                    }
                }
                UciMessage::Stop => {
                    error!("UCI stop command but this is not implemented");
                    unimplemented!("UCI stop command")
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
}
