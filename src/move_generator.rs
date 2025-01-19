use log::{debug, error};

use crate::{
    board::{
        Board, BOARD_SQUARE_INDEX_TRANSLATION_64, CASTLE_BLACK_KING_FLAG, CASTLE_BLACK_QUEEN_FLAG,
        CASTLE_WHITE_KING_FLAG, CASTLE_WHITE_QUEEN_FLAG, COLOR_BLACK, COLOR_FLAG_MASK,
        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION, HASH_VALUES, PIECE_BISHOP, PIECE_INVALID, PIECE_KING,
        PIECE_KNIGHT, PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK,
    },
    evaluate::CENTIPAWN_VALUES,
    moves::{
        Move, MoveRollback, MOVE_DOUBLE_PAWN, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_PROMOTION,
        MOVE_KING_CASTLE, MOVE_PROMO_BISHOP, MOVE_PROMO_KNIGHT, MOVE_PROMO_QUEEN, MOVE_PROMO_ROOK, MOVE_QUEEN_CASTLE,
    },
    search::{HistoryTable, DEFAULT_HISTORY_TABLE},
};

const ENABLE_PERFT_STATS: bool = true;
/// This option is slow
const ENABLE_PERFT_STATS_CHECKS: bool = false;
/// This option is very slow
const ENABLE_PERFT_STATS_CHECKMATES: bool = false;
pub const ENABLE_UNMAKE_MOVE_TEST: bool = false;

/// Has value of target - self added so typical range is +-800. I guess kings capturing have the highest value.
/// Capturing promotions also have value of piece to become added so their additional range is +300 to +1800
const MOVE_SCORE_CAPTURE: i16 = 2000;
pub const MOVE_SCORE_KILLER_1: i16 = 1999;
pub const MOVE_SCORE_KILLER_2: i16 = 1998;
/// Has value of piece is becomes added so really the range is +300 to +900
const MOVE_SCORE_PROMOTION: i16 = 1000;
const MOVE_SCORE_KING_CASTLE: i16 = 502;
const MOVE_SCORE_QUEEN_CASTLE: i16 = 501;
/// No idea what a good value is; only applied to quiet moves. Can also go down to negative this value.
pub const MOVE_SCORE_HISTORY_MAX: i32 = 500;
const MOVE_SCORE_QUIET: i16 = 0;

/// Values from https://www.chessprogramming.org/10x12_Board under TSCP
/// If the piece can slide through squares when moving
const SLIDES: [bool; 5] = [false, true, true, true, false];
#[rustfmt::skip]
/// 10x12 repr offsets for moving in each piece's valid directions
const OFFSET: [[i8; 8]; 5] = [
	[ -21, -19,-12, -8, 8, 12, 19, 21 ], /* KNIGHT */
	[ -11,  -9,  9, 11, 0,  0,  0,  0 ], /* BISHOP */
	[ -10,  -1,  1, 10, 0,  0,  0,  0 ], /* ROOK */
	[ -11, -10, -9, -1, 1,  9, 10, 11 ], /* QUEEN */
	[ -11, -10, -9, -1, 1,  9, 10, 11 ]  /* KING */
];

#[inline]
pub fn generate_moves_with_history(board: &mut Board, history_table: &HistoryTable) -> Vec<ScoredMove> {
    generate_moves::<true, false>(board, history_table)
}

#[inline]
pub fn generate_moves_without_history(board: &mut Board) -> Vec<ScoredMove> {
    generate_moves::<false, false>(board, &DEFAULT_HISTORY_TABLE)
}

#[inline]
pub fn generate_capture_moves(board: &mut Board) -> Vec<ScoredMove> {
    generate_moves::<false, true>(board, &DEFAULT_HISTORY_TABLE)
}

/// if make and unmake move work properly then at the end board should be back to it's original state
pub fn generate_moves<const USE_HISTORY: bool, const ONLY_CAPTURES: bool>(
    board: &mut Board,
    history_table: &HistoryTable,
) -> Vec<ScoredMove> {
    let mut moves = generate_moves_psuedo_legal::<USE_HISTORY, ONLY_CAPTURES>(board, history_table);
    let mut rollback = MoveRollback::default();
    let mut board_copy = None;
    if ENABLE_UNMAKE_MOVE_TEST {
        board_copy = Some(board.clone());
    }

    // if log_enabled!(log::Level::Trace) {
    //     for m in &moves {
    //         trace!("{}", m.pretty_print(Some(board)));
    //     }
    // }

    moves.retain(|r#move| {
        let mut result;
        let flags = r#move.m.flags();

        if flags == MOVE_KING_CASTLE || flags == MOVE_QUEEN_CASTLE {
            // Check the king isn't in check to begin with
            board.white_to_move = !board.white_to_move;
            result = !can_capture_opponent_king(board, false);
            board.white_to_move = !board.white_to_move;

            if !result {
                return result;
            }

            let direction_sign = if flags == MOVE_KING_CASTLE { 1 } else { -1 };
            let from = r#move.m.from();
            let intermediate_index = from.checked_add_signed(direction_sign).unwrap();
            let intermediate_move = Move::new(from as u8, intermediate_index as u8, 0);

            board.make_move(&intermediate_move, &mut rollback);
            result = !can_capture_opponent_king(board, true);
            board.unmake_move(&intermediate_move, &mut rollback);

            if ENABLE_UNMAKE_MOVE_TEST && board_copy.as_ref().unwrap() != board {
                error!("unmake move did not properly undo move {:?}", intermediate_move);
                assert_eq!(board_copy.as_ref().unwrap(), board);
            }

            if !result {
                return result;
            }
        }

        board.make_move(&r#move.m, &mut rollback);
        result = !can_capture_opponent_king(board, true);
        board.unmake_move(&r#move.m, &mut rollback);

        if ENABLE_UNMAKE_MOVE_TEST && board_copy.as_ref().unwrap() != board {
            error!("unmake move did not properly undo move {:?}", r#move.m);

            let board_copied = board_copy.as_ref().unwrap();
            if board_copied.hash != board.hash {
                for (i, v) in HASH_VALUES.iter().enumerate() {
                    if board_copied.hash ^ v == board.hash {
                        debug!("make/unmake differs by value {i} of HASH_VALUES in hash");
                    }
                }
            }

            assert_eq!(board_copy.as_ref().unwrap(), board);
        }

        result
    });

    debug_assert!(rollback.is_empty());

    moves
}

pub fn generate_moves_psuedo_legal<const USE_HISTORY: bool, const ONLY_CAPTURES: bool>(
    board: &Board,
    history_table: &HistoryTable,
) -> Vec<ScoredMove> {
    let mut result = Vec::new();
    let color_flag = if board.white_to_move { 0 } else { COLOR_BLACK };
    let history_color_value = if board.white_to_move { 0 } else { 1 };
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
                                if !ONLY_CAPTURES {
                                    // if log_enabled!(log::Level::Trace) {
                                    //     trace!(
                                    //         "{} {} {:#04x} {} {} {} {:#04x} {}",
                                    //         i,
                                    //         index_10x12_to_pos_str(i as u8),
                                    //         piece,
                                    //         piece_to_name(piece),
                                    //         cur_pos,
                                    //         index_10x12_to_pos_str(cur_pos as u8),
                                    //         target_piece,
                                    //         piece_to_name(target_piece)
                                    //     );
                                    // }

                                    // Doing this extra translation is probably bad for performance. May as well use 3 bytes per move instead of 2?
                                    result.push(ScoredMove::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[cur_pos],
                                        0,
                                        MOVE_SCORE_QUIET
                                            + if USE_HISTORY {
                                                history_table[history_color_value][piece_type - 1]
                                                    [DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[cur_pos] as usize]
                                            } else {
                                                0
                                            },
                                    ));
                                }
                            } else {
                                if target_piece & COLOR_FLAG_MASK != color_flag {
                                    let score_diff = CENTIPAWN_VALUES[(target_piece & PIECE_MASK) as usize]
                                        - CENTIPAWN_VALUES[(piece & PIECE_MASK) as usize];

                                    // if log_enabled!(log::Level::Trace) {
                                    //     trace!(
                                    //         "{} {} {:#04x} {} {} {} {:#04x} {}",
                                    //         i,
                                    //         index_10x12_to_pos_str(i as u8),
                                    //         piece,
                                    //         piece_to_name(piece),
                                    //         cur_pos,
                                    //         index_10x12_to_pos_str(cur_pos as u8),
                                    //         target_piece,
                                    //         piece_to_name(target_piece)
                                    //     );
                                    // }

                                    result.push(ScoredMove::new(
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[cur_pos],
                                        MOVE_FLAG_CAPTURE,
                                        MOVE_SCORE_CAPTURE + score_diff,
                                    ));
                                }
                                break;
                            }

                            if !SLIDES[piece_type - 2] {
                                break;
                            }
                        }
                    }

                    if piece_type == PIECE_KING as usize && !ONLY_CAPTURES {
                        for offset in [-1, 1] {
                            if (offset == -1
                                && board.white_to_move
                                && board.castling_rights & CASTLE_WHITE_QUEEN_FLAG != 0)
                                || (offset == -1
                                    && !board.white_to_move
                                    && board.castling_rights & CASTLE_BLACK_QUEEN_FLAG != 0)
                                || (offset == 1
                                    && board.white_to_move
                                    && board.castling_rights & CASTLE_WHITE_KING_FLAG != 0)
                                || (offset == 1
                                    && !board.white_to_move
                                    && board.castling_rights & CASTLE_BLACK_KING_FLAG != 0)
                            {
                                let friendly_rook = PIECE_ROOK | color_flag;
                                let mut cur_pos = i;
                                loop {
                                    cur_pos = cur_pos.checked_add_signed(offset).unwrap();
                                    let target_piece = board.get_piece(cur_pos);

                                    if target_piece != PIECE_NONE {
                                        if target_piece == friendly_rook
                                            && (cur_pos == 21 || cur_pos == 28 || cur_pos == 91 || cur_pos == 98)
                                        {
                                            let king_to = i.checked_add_signed(offset * 2).unwrap();

                                            let (flags, score) = if offset == -1 {
                                                (MOVE_QUEEN_CASTLE, MOVE_SCORE_QUEEN_CASTLE)
                                            } else {
                                                (MOVE_KING_CASTLE, MOVE_SCORE_KING_CASTLE)
                                            };
                                            result.push(ScoredMove::new(
                                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                                DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[king_to],
                                                flags,
                                                score,
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
                    let can_promo =
                        (!board.white_to_move && i > 30 && i < 40) || (board.white_to_move && i > 80 && i < 900);

                    if !ONLY_CAPTURES {
                        let mut target_pos = i.checked_add_signed(10 * direction_sign).unwrap();
                        let mut target_piece = board.get_piece(target_pos);
                        if target_piece == PIECE_NONE {
                            if can_promo {
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    MOVE_PROMO_QUEEN,
                                    MOVE_SCORE_PROMOTION + CENTIPAWN_VALUES[PIECE_QUEEN as usize],
                                ));
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    MOVE_PROMO_ROOK,
                                    MOVE_SCORE_PROMOTION + CENTIPAWN_VALUES[PIECE_ROOK as usize],
                                ));
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    MOVE_PROMO_BISHOP,
                                    MOVE_SCORE_PROMOTION + CENTIPAWN_VALUES[PIECE_BISHOP as usize],
                                ));
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    MOVE_PROMO_KNIGHT,
                                    MOVE_SCORE_PROMOTION + CENTIPAWN_VALUES[PIECE_KNIGHT as usize],
                                ));
                            } else {
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    0,
                                    MOVE_SCORE_QUIET
                                        + if USE_HISTORY {
                                            history_table[history_color_value][PIECE_PAWN as usize - 1]
                                                [DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos] as usize]
                                        } else {
                                            0
                                        },
                                ));

                                // On starting rank?
                                if (board.white_to_move && i > 30 && i < 40)
                                    || (!board.white_to_move && i > 80 && i < 90)
                                {
                                    target_pos = target_pos.checked_add_signed(10 * direction_sign).unwrap();
                                    target_piece = board.get_piece(target_pos);

                                    if target_piece == PIECE_NONE {
                                        result.push(ScoredMove::new(
                                            DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                            DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                            MOVE_DOUBLE_PAWN,
                                            MOVE_SCORE_QUIET
                                                + if USE_HISTORY {
                                                    history_table[history_color_value][PIECE_PAWN as usize - 1]
                                                        [DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos]
                                                            as usize]
                                                } else {
                                                    0
                                                },
                                        ));
                                    }
                                }
                            }
                        }
                    }

                    for offset in [9, 11] {
                        let target_pos = i.checked_add_signed(offset * direction_sign).unwrap();
                        let target_piece = board.get_piece(target_pos);

                        if target_piece != PIECE_INVALID
                            && ((target_piece != PIECE_NONE && target_piece & COLOR_FLAG_MASK != color_flag)
                                || (target_pos == ep_target && target_piece == PIECE_NONE))
                        {
                            let score = MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES[(target_piece & PIECE_MASK) as usize]
                                - CENTIPAWN_VALUES[PIECE_PAWN as usize];
                            if can_promo {
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    MOVE_PROMO_QUEEN | MOVE_FLAG_CAPTURE,
                                    score + CENTIPAWN_VALUES[PIECE_QUEEN as usize],
                                ));
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    MOVE_PROMO_ROOK | MOVE_FLAG_CAPTURE,
                                    score + CENTIPAWN_VALUES[PIECE_ROOK as usize],
                                ));
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    MOVE_PROMO_BISHOP | MOVE_FLAG_CAPTURE,
                                    score + CENTIPAWN_VALUES[PIECE_BISHOP as usize],
                                ));
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    MOVE_PROMO_KNIGHT | MOVE_FLAG_CAPTURE,
                                    score + CENTIPAWN_VALUES[PIECE_KNIGHT as usize],
                                ));
                            } else {
                                result.push(ScoredMove::new(
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i],
                                    DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[target_pos],
                                    if target_pos == ep_target {
                                        MOVE_EP_CAPTURE
                                    } else {
                                        MOVE_FLAG_CAPTURE
                                    },
                                    score,
                                ));
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

pub fn can_capture_opponent_king(board: &Board, is_legality_test_after_move: bool) -> bool {
    let mut king_pos_opt = None;
    if board.white_to_move {
        let opponent_king_piece = PIECE_KING | COLOR_BLACK;
        for i in (0..64).rev() {
            if board.get_piece_64(i) == opponent_king_piece {
                king_pos_opt = Some(i);
                break;
            }
        }
    } else {
        let opponent_king_piece = PIECE_KING;
        for i in 0..64 {
            if board.get_piece_64(i) == opponent_king_piece {
                king_pos_opt = Some(i);
                break;
            }
        }
    }

    if king_pos_opt.is_none() {
        if is_legality_test_after_move {
            // The king is gone, checkmate already happened
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
    let king = PIECE_KING | color_flag;
    for offset in OFFSET[PIECE_ROOK as usize - 2] {
        let mut current_pos = king_pos;
        let mut first = true;
        loop {
            current_pos = current_pos.checked_add_signed(offset).unwrap();
            let piece = board.get_piece(current_pos as usize);
            if piece != PIECE_NONE {
                if piece == queen || piece == rook || (first && piece == king) {
                    return true;
                }
                break;
            }
            first = false;
        }
    }

    let bishop = PIECE_BISHOP | color_flag;
    for offset in OFFSET[PIECE_BISHOP as usize - 2] {
        let mut current_pos = king_pos;
        let mut first = true;
        loop {
            current_pos = current_pos.checked_add_signed(offset).unwrap();
            let piece = board.get_piece(current_pos as usize);
            if piece != PIECE_NONE {
                if piece == queen || piece == bishop || (first && piece == king) {
                    return true;
                }
                break;
            }
            first = false;
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
    let direction_sign = if board.white_to_move { -1 } else { 1 };
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
pub fn perft(depth: u8, board: &mut Board, rollback: &mut MoveRollback, stats: &mut PerftStats) {
    if depth == 0 {
        // slow as all heck
        if ENABLE_PERFT_STATS && ENABLE_PERFT_STATS_CHECKMATES && generate_moves_without_history(board).is_empty() {
            stats.checkmates += 1;
        }

        stats.nodes += 1;
        return;
    }

    let moves = generate_moves_without_history(board);
    for r#move in &moves {
        board.make_move(&r#move.m, rollback);

        if ENABLE_PERFT_STATS && depth == 1 {
            check_perft_stats(&r#move.m, board, stats);
        }

        perft(depth - 1, board, rollback, stats);
        board.unmake_move(&r#move.m, rollback);
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
    if ENABLE_PERFT_STATS_CHECKS && can_capture_opponent_king(board, false) {
        stats.checks += 1;
    }
    board.white_to_move = !board.white_to_move;
}

pub struct ScoredMove {
    pub m: Move,
    pub score: i16,
}

impl ScoredMove {
    pub fn new(from_square_index: u8, to_square_index: u8, flags: u16, score: i16) -> Self {
        Self {
            m: Move::new(from_square_index, to_square_index, flags),
            score,
        }
    }
}
