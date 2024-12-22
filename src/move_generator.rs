use crate::{
    board::{
        Board, BOARD_SQUARE_INDEX_TRANSLATION_64, CASTLE_BLACK_KING, CASTLE_BLACK_QUEEN, CASTLE_WHITE_KING,
        CASTLE_WHITE_QUEEN, COLOR_BLACK, COLOR_FLAG_MASK, DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION,
        PIECE_INVALID, PIECE_KING, PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_ROOK,
    },
    moves::{
        Move, MOVE_DOUBLE_PAWN, MOVE_FLAG_CAPTURE, MOVE_KING_CASTLE, MOVE_PROMO_BISHOP, MOVE_PROMO_KNIGHT,
        MOVE_PROMO_QUEEN, MOVE_PROMO_ROOK, MOVE_QUEEN_CASTLE,
    },
};

// Values from https://www.chessprogramming.org/10x12_Board under TSCP
// If the piece can slide through squares when moving
const SLIDES: [bool; 6] = [false, false, true, true, true, false];
#[rustfmt::skip]
// 10x12 repr offsets for moving in each piece's valid directions
const OFFSET: [[i8; 8]; 6] = [
	[   0,   0,  0,  0, 0,  0,  0,  0 ],
	[ -21, -19,-12, -8, 8, 12, 19, 21 ], /* KNIGHT */
	[ -11,  -9,  9, 11, 0,  0,  0,  0 ], /* BISHOP */
	[ -10,  -1,  1, 10, 0,  0,  0,  0 ], /* ROOK */
	[ -11, -10, -9, -1, 1,  9, 10, 11 ], /* QUEEN */
	[ -11, -10, -9, -1, 1,  9, 10, 11 ]  /* KING */
];

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
                    for offset in OFFSET[piece_type - 1] {
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
                                // Doing this extra translation is probably bad for performance. May as well use 3 bytes per move instead of 2?
                                result.push(Move::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[cur_pos],
                                    0,
                                ));
                            } else {
                                if target_piece & COLOR_FLAG_MASK != color_flag {
                                    result.push(Move::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[cur_pos],
                                        MOVE_FLAG_CAPTURE,
                                    ));
                                }
                                break;
                            }

                            if !SLIDES[piece_type - 1] {
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
                                        MOVE_FLAG_CAPTURE,
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
