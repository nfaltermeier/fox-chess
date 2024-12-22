use crate::{
    board::{
        index_10x12_to_pos_str, piece_to_name, Board, BOARD_SQUARE_INDEX_TRANSLATION_64, CASTLE_BLACK_KING,
        CASTLE_BLACK_QUEEN, CASTLE_WHITE_KING, CASTLE_WHITE_QUEEN, COLOR_BLACK, COLOR_FLAG_MASK,
        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION, PIECE_BISHOP, PIECE_INVALID, PIECE_KING, PIECE_KNIGHT,
        PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK,
    },
    moves::{
        Move, MoveRollback, MOVE_DOUBLE_PAWN, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_PROMOTION,
        MOVE_KING_CASTLE, MOVE_PROMO_BISHOP, MOVE_PROMO_KNIGHT, MOVE_PROMO_QUEEN, MOVE_PROMO_ROOK, MOVE_QUEEN_CASTLE,
    },
};

const ENABLE_PERFT_STATS: bool = true;

// Values from https://www.chessprogramming.org/10x12_Board under TSCP
// If the piece can slide through squares when moving
const SLIDES: [bool; 5] = [false, true, true, true, false];
#[rustfmt::skip]
// 10x12 repr offsets for moving in each piece's valid directions
const OFFSET: [[i8; 8]; 5] = [
	[ -21, -19,-12, -8, 8, 12, 19, 21 ], /* KNIGHT */
	[ -11,  -9,  9, 11, 0,  0,  0,  0 ], /* BISHOP */
	[ -10,  -1,  1, 10, 0,  0,  0,  0 ], /* ROOK */
	[ -11, -10, -9, -1, 1,  9, 10, 11 ], /* QUEEN */
	[ -11, -10, -9, -1, 1,  9, 10, 11 ]  /* KING */
];

// if make and unmake move work properly then at the end board should be back to it's original state
pub fn generate_moves(board: &mut Board) -> Vec<Move> {
    let mut moves = generate_moves_psuedo_legal(board);
    let mut rollback = MoveRollback::default();

    moves.retain(|r#move| {
        board.make_move(r#move, &mut rollback);
        let result = !can_capture_opponent_king(board, true);
        board.unmake_move(r#move, &mut rollback);

        result
    });

    debug_assert!(rollback.is_empty());

    moves
}

pub fn generate_moves_psuedo_legal(board: &Board) -> Vec<Move> {
    let mut result = Vec::new();
    let color_flag = if board.white_to_move { 0 } else { COLOR_BLACK };
    let ep_target = match board.en_passant_target_square_index {
        Some(ep_target_untranslated) => BOARD_SQUARE_INDEX_TRANSLATION_64[ep_target_untranslated as usize],
        None => 0xFF,
    } as usize;

    let mut i: usize = 21;
    for _ in 0..8 {
        for _ in 0..8 {
            let piece = board.get_piece(i);

            debug_assert!(piece != PIECE_INVALID);
            if piece != PIECE_NONE && piece & COLOR_FLAG_MASK == color_flag {
                let piece_type = (piece & PIECE_MASK) as usize;
                if piece_type != PIECE_PAWN as usize {
                    for offset in OFFSET[piece_type - 2] {
                        if offset == 0 {
                            break;
                        }

                        let mut cur_pos = i;
                        loop {
                            cur_pos = cur_pos.checked_add_signed(offset as isize).unwrap();
                            let target_piece = board.get_piece(cur_pos);

                            if target_piece == PIECE_INVALID {
                                break;
                            }

                            if target_piece == PIECE_NONE {
                                // println!("{} {} {:#04x} {} {} {} {:#04x} {}", i, index_10x12_to_pos_str(i as u8), piece, piece_to_name(piece), cur_pos, index_10x12_to_pos_str(cur_pos as u8), target_piece, piece_to_name(target_piece));

                                // Doing this extra translation is probably bad for performance. May as well use 3 bytes per move instead of 2?
                                result.push(Move::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[cur_pos],
                                    0,
                                ));
                            } else {
                                if target_piece & COLOR_FLAG_MASK != color_flag {
                                    // println!("{} {} {:#04x} {} {} {} {:#04x} {}", i, index_10x12_to_pos_str(i as u8), piece, piece_to_name(piece), cur_pos, index_10x12_to_pos_str(cur_pos as u8), target_piece, piece_to_name(target_piece));

                                    result.push(Move::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[cur_pos],
                                        MOVE_FLAG_CAPTURE,
                                    ));
                                }
                                break;
                            }

                            if !SLIDES[piece_type - 2] {
                                break;
                            }
                        }
                    }

                    if piece_type == PIECE_KING as usize {
                        for offset in [-1, 1] {
                            if (offset == -1 && board.white_to_move && board.castling_rights & CASTLE_WHITE_QUEEN != 0)
                                || (offset == -1
                                    && !board.white_to_move
                                    && board.castling_rights & CASTLE_BLACK_QUEEN != 0)
                                || (offset == 1
                                    && board.white_to_move
                                    && board.castling_rights & CASTLE_WHITE_KING != 0)
                                || (offset == 1
                                    && !board.white_to_move
                                    && board.castling_rights & CASTLE_BLACK_KING != 0)
                            {
                                let friendly_rook = PIECE_ROOK | color_flag;
                                let mut cur_pos = i;
                                loop {
                                    cur_pos = cur_pos.checked_add_signed(offset).unwrap();
                                    let target_piece = board.get_piece(cur_pos);

                                    if target_piece != PIECE_NONE {
                                        if target_piece == friendly_rook {
                                            // When castling to pos will be the rook's position
                                            let flags = if offset == -1 {
                                                MOVE_QUEEN_CASTLE
                                            } else {
                                                MOVE_KING_CASTLE
                                            };
                                            result.push(Move::new(
                                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[cur_pos],
                                                flags,
                                            ));
                                        }

                                        break;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // Do pawn movement
                    let direction_sign: isize = if board.white_to_move { 1 } else { -1 };
                    let mut target_pos = i.checked_add_signed(10 * direction_sign).unwrap();
                    let mut target_piece = board.get_piece(target_pos);
                    let can_promo = (target_pos > 20 && target_pos < 30) || (target_pos > 90 && target_pos < 100);

                    if target_piece == PIECE_NONE {
                        if can_promo {
                            result.push(Move::new(
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                MOVE_PROMO_KNIGHT,
                            ));
                            result.push(Move::new(
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                MOVE_PROMO_BISHOP,
                            ));
                            result.push(Move::new(
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                MOVE_PROMO_ROOK,
                            ));
                            result.push(Move::new(
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                MOVE_PROMO_QUEEN,
                            ));
                        } else {
                            result.push(Move::new(
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                0,
                            ));

                            // On starting rank?
                            if (board.white_to_move && i > 30 && i < 40) || (!board.white_to_move && i > 80 && i < 90) {
                                target_pos = target_pos.checked_add_signed(10 * direction_sign).unwrap();
                                target_piece = board.get_piece(target_pos);

                                if target_piece == PIECE_NONE {
                                    result.push(Move::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                        MOVE_DOUBLE_PAWN,
                                    ));
                                }
                            }
                        }
                    }

                    for offset in [9, 11] {
                        target_pos = i.checked_add_signed(offset * direction_sign).unwrap();
                        target_piece = board.get_piece(target_pos);

                        if target_piece != PIECE_INVALID {
                            if (target_piece != PIECE_NONE && target_piece & COLOR_FLAG_MASK != color_flag)
                                || (target_pos == ep_target && target_piece == PIECE_NONE)
                            {
                                if can_promo {
                                    result.push(Move::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                        MOVE_PROMO_KNIGHT | MOVE_FLAG_CAPTURE,
                                    ));
                                    result.push(Move::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                        MOVE_PROMO_BISHOP | MOVE_FLAG_CAPTURE,
                                    ));
                                    result.push(Move::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                        MOVE_PROMO_ROOK | MOVE_FLAG_CAPTURE,
                                    ));
                                    result.push(Move::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                        MOVE_PROMO_QUEEN | MOVE_FLAG_CAPTURE,
                                    ));
                                } else {
                                    result.push(Move::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                        if target_pos == ep_target {
                                            MOVE_EP_CAPTURE
                                        } else {
                                            MOVE_FLAG_CAPTURE
                                        },
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            i += 1;
        }
        // The inner loop will still increment so 1 + 2 = 3 which moves to the first item of the next row
        i += 2;
    }

    result
}

fn can_capture_opponent_king(board: &Board, is_legality_test_after_move: bool) -> bool {
    let mut king_pos_opt = None;
    let opponent_color = if board.white_to_move { COLOR_BLACK } else { 0 };
    let opponent_king_piece = PIECE_KING | opponent_color;
    for i in 0..64 {
        if board.get_piece_64(i) == opponent_king_piece {
            king_pos_opt = Some(i);
            break;
        }
    }

    if king_pos_opt.is_none() {
        if is_legality_test_after_move {
            // The king is gone, checkmate alredy happened
            return true;
        } else {
            panic!("Could not find opponent king in can_capture_opponent_king")
        }
    }
    // Translate from 8x8 to 10x12 board index too
    let king_pos = BOARD_SQUARE_INDEX_TRANSLATION_64[king_pos_opt.unwrap()];

    let color_flag = if board.white_to_move { 0 } else { COLOR_BLACK };

    let queen = PIECE_QUEEN | color_flag;
    let rook = PIECE_ROOK | color_flag;
    for offset in OFFSET[PIECE_ROOK as usize - 2] {
        let mut current_pos = king_pos;
        loop {
            current_pos = current_pos.checked_add_signed(offset).unwrap();
            let piece = board.get_piece(current_pos as usize);
            if piece != PIECE_NONE {
                if piece == queen || piece == rook {
                    return true;
                }
                break;
            }
        }
    }

    let bishop = PIECE_BISHOP | color_flag;
    for offset in OFFSET[PIECE_BISHOP as usize - 2] {
        let mut current_pos = king_pos;
        loop {
            current_pos = current_pos.checked_add_signed(offset).unwrap();
            let piece = board.get_piece(current_pos as usize);
            if piece != PIECE_NONE {
                if piece == queen || piece == bishop {
                    return true;
                }
                break;
            }
        }
    }

    let knight = PIECE_KNIGHT | color_flag;
    for offset in OFFSET[PIECE_KNIGHT as usize - 2] {
        let target_pos = king_pos.checked_add_signed(offset).unwrap() as usize;
        let piece = board.get_piece(target_pos);
        if piece == knight {
            return true;
        }
    }

    let pawn = PIECE_PAWN | color_flag;
    let direction_sign = if board.white_to_move { 1 } else { -1 };
    for offset in [9, 11] {
        let target_pos = king_pos.checked_add_signed(offset * direction_sign).unwrap() as usize;
        let piece = board.get_piece(target_pos);
        if piece == pawn {
            return true;
        }
    }

    false
}

#[derive(Debug, Default)]
pub struct PerftStats {
    pub nodes: u64,
    pub captures: u64,
    pub eps: u64,
    pub castles: u64,
    pub promotions: u64,
    pub checks: u64,
    pub checkmates: u64,
}

// Code referenced from https://www.chessprogramming.org/Perft
pub fn perft_pseudo_legal_optimized(depth: u8, board: &mut Board, rollback: &mut MoveRollback, stats: &mut PerftStats) {
    if depth == 0 {
        stats.nodes += 1;
        return;
    }

    let moves = generate_moves_psuedo_legal(board);
    for r#move in moves {
        board.make_move(&r#move, rollback);
        if !can_capture_opponent_king(board, true) {
            if ENABLE_PERFT_STATS && depth == 1 {
                check_perft_stats(&r#move, board, stats);
            }

            perft_pseudo_legal_optimized(depth - 1, board, rollback, stats);
        }
        board.unmake_move(&r#move, rollback);
    }
}

pub fn perft(depth: u8, board: &mut Board, rollback: &mut MoveRollback, stats: &mut PerftStats) {
    if depth == 0 {
        // Causes incorrect results
        // if ENABLE_PERFT_STATS && generate_moves(board).is_empty() {
        //     stats.checkmates += 1;
        // }

        stats.nodes += 1;
        return;
    }

    let moves = generate_moves(board);
    for r#move in &moves {
        board.make_move(&r#move, rollback);

        if ENABLE_PERFT_STATS && depth == 1 {
            check_perft_stats(&r#move, board, stats);
        }

        perft(depth - 1, board, rollback, stats);
        board.unmake_move(&r#move, rollback);
    }
}

fn check_perft_stats(r#move: &Move, board: &mut Board, stats: &mut PerftStats) {
    let flags = r#move.data >> 12;
    if flags & MOVE_FLAG_CAPTURE != 0 {
        stats.captures += 1;

        if flags == MOVE_EP_CAPTURE {
            stats.eps += 1;
        }
    } else if flags == MOVE_KING_CASTLE || flags == MOVE_QUEEN_CASTLE {
        stats.castles += 1;
    }

    if flags & MOVE_FLAG_PROMOTION != 0 {
        stats.promotions += 1;
    }

    board.white_to_move = !board.white_to_move;
    if can_capture_opponent_king(board, false) {
        stats.checks += 1;
    }
    board.white_to_move = !board.white_to_move;
}
