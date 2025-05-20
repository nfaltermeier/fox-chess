use std::cell::Cell;

use array_macro::array;
use rand::random;

use crate::{bitboard::{north_fill, south_fill, LIGHT_SQUARES}, board::{
    file_8x8, Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_MASK,
    PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK,
}};

// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES: [i16; 7] = [0, 81, 309, 338, 501, 1021, 20000];

pub static GAME_STAGE_VALUES: [i16; 7] = [0, 0, 4, 4, 4, 8, 0];
pub const MAX_GAME_STAGE: i16 = 16 * GAME_STAGE_VALUES[PIECE_PAWN as usize]
    + 4 * GAME_STAGE_VALUES[PIECE_KNIGHT as usize]
    + 4 * GAME_STAGE_VALUES[PIECE_BISHOP as usize]
    + 4 * GAME_STAGE_VALUES[PIECE_ROOK as usize]
    + 2 * GAME_STAGE_VALUES[PIECE_QUEEN as usize]
    + 2 * GAME_STAGE_VALUES[PIECE_KING as usize];
pub const MIN_GAME_STAGE_FULLY_MIDGAME: i16 = GAME_STAGE_VALUES[PIECE_ROOK as usize] * 2
    + GAME_STAGE_VALUES[PIECE_BISHOP as usize] * 3
    + GAME_STAGE_VALUES[PIECE_KNIGHT as usize] * 3;
pub const ENDGAME_GAME_STAGE_FOR_QUIESCENSE: i16 =
    GAME_STAGE_VALUES[PIECE_BISHOP as usize] * 2 + GAME_STAGE_VALUES[PIECE_ROOK as usize] * 2;

pub const MATE_THRESHOLD: i16 = 20000;
pub const MATE_VALUE: i16 = 25000;

#[rustfmt::skip]
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 138, 174, 122,  87,  78,  58,  13,  80,
  32,  58,  40,  46,  57,  59,  95,  10,
 -10,   4,   0,  13,  17,  15,   4,  -3,
 -14,   7,  -7,   5,  -4,   4,   0, -15,
 -23,   8,   0,  -5,  -1,   5,  23,  -2,
 -29,  -4, -17, -10,  -9,   7,  26, -18,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 221, 237, 255, 153, 153, 236, 237, 225,
 100, 132, 119,  88,  68,  32,  66,  69,
  64,  63,  52,  -6, -14,   1,  40,  12,
  26,  26,  14, -25,  -5,  12,  21,   7,
  29,  34,  12,  21,  10,  13,   3,  -1,
  40,  41,  23,  93,   4,  18,  -5,   4,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-157, -13, -14, -13,  33, -45, -40,-102,
 -29,   0,  21,  54,  35,  41, -26,   7,
  -8,  23,  40,  61,  85,  87,  42,  13,
  10,   7,  26,  49,  17,  50,  14,  36,
  -1,  -3,  15,   7,  19,  24,  25,  -3,
  -1,  -5,   4,  16,  25,   2,  14,  -8,
 -45, -15,  -1,  -2,   0,   3, -26, -14,
 -76, -12, -38, -33, -17,  -9, -12, -41,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -50, -35, -28, -37, -33, -30, -40, -55,
 -50, -28, -61, -42, -32, -71, -22, -70,
 -15,  -5, -44, -58,-103, -56, -29, -65,
 -72, -31, -48, -48, -16, -49, -39, -77,
 -59, -41, -13,  -7, -17, -39, -24, -56,
 -84, -55, -41, -49, -37, -38, -71, -69,
 -44, -20, -49, -66, -56, -72, -43,-147,
-105, -84, -44, -71, -93, -60, -41, -51,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -26, -32, -22, -30, -13, -48,   2,  -6,
 -21, -22,   9,  13,   3,   3,   6, -27,
  -8,   2,  21,  32,  41,  67,  21,  27,
  17,  -4,  12,  29,  28,  17,  -4,   9,
  -8,   9,   3,  18,  18,   6,   5,  -2,
   3,   5,  12,   6,   7,   2,   1,  20,
   5,  -8,   8,  -6,   0,  -2,   9, -31,
 -48, -22, -18, -11, -15, -15,  -7, -25,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -20, -25, -18, -10, -10, -10, -10, -20,
  -1, -12, -44, -39, -13, -34, -26, -43,
 -23,   2, -32, -67, -63, -53, -19, -61,
 -55, -15, -34, -34, -29, -23, -21, -47,
 -33, -23,  -8, -11, -30, -36, -35, -79,
 -85, -35,  -9, -35, -17, -31, -88, -96,
 -92, -62, -91, -44, -63, -51,-102,-185,
 -38, -25, -90, -62, -43, -68, -56, -39,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  38,  63,  58,  49,  73,  76,  83,  14,
  41,  38,  71,  71,  64,  94,  63,  58,
  22,  67,  60,  49,  47,  68,  77,  49,
  18,  12,  19,  26,  39,  36,  42,  15,
 -11,   7,   2,  16,  12,   8,  22,  -3,
 -12,   4,   0,   1,  -2,   2,  18,   3,
 -26, -12,  -1,  -3,  -8,   5,   2, -37,
 -14,  -1,   9,   9,   8,   0, -19, -42,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  34,  16,  17,  30,  -4,   1,  17,  37,
  32,  36,  11,  19,  16, -11,  19,  -1,
  38,   1,  26,  21,  19,   1,  10,  -2,
  26,  47,  51,  38,  18,  19,  30,  14,
  42,  21,  52,  26,  32,  35,   0,  17,
  14,   0,  16,  23,  26,  16, -28, -17,
   9,  17,  27,  11,  17, -17, -20,   0,
   8,   6,  17,  34,   1,   2,  33,  11,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,  10,  33,  51,  59,  81,  98,  38,
 -17, -31,  11,  16,  24,  77,  36,  68,
   0,   8,   6,  27,  44,  97,  78,  63,
  -4,  -6,   1,  22,  32,  26,  48,  30,
  -2,  -5,   0,   5,   8,  14,  20,  13,
 -11,   3,   4,   5,   9,  11,  21,   4,
 -27,  -6,   2,   0,   7,   6, -13,  -8,
  11, -14,  -9,   0,  -6, -27, -30, -54,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  15,  72,  53,  72,  -2,   8,  -5, -20,
  44, 110, 115, 100, 142,  90,  78,   2,
  13,   1,  82,  74, 106,  50,   1,  -9,
  69,  60,  86,  72, 121, 107,  57,  22,
  31,  54,  79,  70, 110,  98,  23,  -5,
  -8,  34,  89,  45,  32,  69,   5, -10,
 -10,   0,  45,   8,  11, -26, -45, -10,
 -20, -30, -10,   9,  -5, -11, -10, -20,
];

#[rustfmt::skip]
/// This appears to have some anomalous values
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -30, -39, -40, -50, -47, -39, -40, -30,
 -30, -30, -29, -50, -50, -40,  -2, -30,
 -30,  31, -13, -23,  13,  66, 123,  14,
 -30,  57,  11,  15,  40,  99, 123,  31,
 -18,  26,  32,  31,  18,  13, -32,   3,
 -12,  30,  18,  10,   3,   1,   0, -27,
  10,  19,   7, -21, -10,  -9,  13,  16,
 -10,  40,  14, -34,  19, -33,  26,  20,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -50, -36, -30, -20, -15,  22, -20, -50,
 -30,  46,   2,  47,  53,  69,  45,  -2,
  -4,  24,  35,  37,  37,  18,   3,  41,
 -25, -12,  21,  16,   9, -14, -34,  -3,
 -30,  -3,   1,   8,  18,  24,  14,   7,
   1, -16,   0,   5,  21,  18,   8,   6,
 -46,  -9,   0,  20,  10,  13,  -6, -23,
 -29, -62, -26, -27, -70,  -8, -54, -68,
];

const ALL_PIECE_SQUARE_TABLES: [[i16; 64]; 12] = [
    PAWN_MIDGAME_SQUARE_TABLE,
    KNIGHT_MIDGAME_SQUARE_TABLE,
    BISHOP_MIDGAME_SQUARE_TABLE,
    ROOK_MIDGAME_SQUARE_TABLE,
    QUEEN_MIDGAME_SQUARE_TABLE,
    KING_MIDGAME_SQUARE_TABLE,
    PAWN_ENDGAME_SQUARE_TABLE,
    KNIGHT_ENDGAME_SQUARE_TABLE,
    BISHOP_ENDGAME_SQUARE_TABLE,
    ROOK_ENDGAME_SQUARE_TABLE,
    QUEEN_ENDGAME_SQUARE_TABLE,
    KING_ENDGAME_SQUARE_TABLE,
];

static PIECE_SQUARE_TABLES: [[[i16; 64]; 12]; 2] = [
    // vertically flip each table for white
    array![x => array![y => ALL_PIECE_SQUARE_TABLES[x][y ^ 0b00111000]; 64]; 12],
    // Evaluate from white's perspective so negate each score for black
    array![x => array![y => -ALL_PIECE_SQUARE_TABLES[x][y]; 64]; 12],
];

// thread_local! {
//     pub static ISOLATED_PAWN_PENALTY: Cell<i16> = const { Cell::new(35) };
//     pub static DOUBLED_PAWN_PENALTY: Cell<i16> = const { Cell::new(25) };
// }

impl Board {
    pub fn evaluate(&self) -> i16 {
        let mut material_score = 0;
        let mut position_score_midgame = 0;
        let mut position_score_endgame = 0;
        let mut game_stage = self.game_stage;

        // Has an added file on each side to avoid bounds checks
        let mut pawn_count = [[0_u8; 10]; 2];

        // white then black
        let mut piece_counts = [[0i8; 7]; 2];
        for i in 0..64 {
            let piece = self.get_piece_64(i);
            if piece != PIECE_NONE {
                let color = (piece & COLOR_FLAG_MASK == COLOR_BLACK) as usize;
                let piece_type = (piece & PIECE_MASK) as usize;
                piece_counts[color][piece_type] += 1;

                position_score_midgame += PIECE_SQUARE_TABLES[color][piece_type - 1][i];
                position_score_endgame += PIECE_SQUARE_TABLES[color][piece_type - 1 + 6][i];

                if piece_type == PIECE_PAWN as usize {
                    let file = file_8x8(i as u8) as usize;
                    pawn_count[color][file + 1] += 1;
                }
            }
        }

        for i in 1..7 {
            material_score += CENTIPAWN_VALUES[i] * (piece_counts[0][i] - piece_counts[1][i]) as i16;
        }

        // positive value: black has more isolated pawns than white
        let mut isolated_pawns = 0;
        let mut doubled_pawns = 0;
        for i in 1..9 {
            isolated_pawns -= (pawn_count[0][i - 1] == 0 && pawn_count[0][i] != 0 && pawn_count[0][i + 1] == 0) as i16;
            isolated_pawns += (pawn_count[1][i - 1] == 0 && pawn_count[1][i] != 0 && pawn_count[1][i + 1] == 0) as i16;
            doubled_pawns -= (pawn_count[0][i] > 1) as i16;
            doubled_pawns += (pawn_count[1][i] > 1) as i16;
        }

        if game_stage > MIN_GAME_STAGE_FULLY_MIDGAME {
            game_stage = MIN_GAME_STAGE_FULLY_MIDGAME;
        }

        let position_score_final = ((position_score_midgame * game_stage)
            + (position_score_endgame * (MIN_GAME_STAGE_FULLY_MIDGAME - game_stage)))
            / (MIN_GAME_STAGE_FULLY_MIDGAME);

        let white_passed = self.white_passed_pawns();
        let white_passed_distance = (south_fill(white_passed) &!white_passed).count_ones();
        
        let black_passed = self.black_passed_pawns();
        let black_passed_distance = (north_fill(black_passed) &!black_passed).count_ones();

        let net_passed_pawns = (white_passed_distance - black_passed_distance) as i16;

        // Add a small variance to try to avoid repetition
        // isolated_pawns * ISOLATED_PAWN_PENALTY.get()
        material_score + position_score_final + doubled_pawns * 14 + net_passed_pawns * 9
    }

    pub fn evaluate_checkmate(&self, ply: u8) -> i16 {
        if self.white_to_move {
            -MATE_VALUE + (ply as i16) * 10
        } else {
            MATE_VALUE - (ply as i16) * 10
        }
    }

    pub fn evaluate_side_to_move_relative(&self) -> i16 {
        self.evaluate() * if self.white_to_move { 1 } else { -1 }
    }

    pub fn evaluate_checkmate_side_to_move_relative(&self, ply: u8) -> i16 {
        self.evaluate_checkmate(ply) * if self.white_to_move { 1 } else { -1 }
    }

    /// Returns true if this position will be called a draw by the arbiter
    pub fn is_insufficient_material(&self) -> bool {
        if self.piece_counts[0][PIECE_QUEEN as usize] == 0
            && self.piece_counts[0][PIECE_ROOK as usize] == 0
            && self.piece_counts[0][PIECE_PAWN as usize] == 0
            && self.piece_counts[1][PIECE_QUEEN as usize] == 0
            && self.piece_counts[1][PIECE_ROOK as usize] == 0
            && self.piece_counts[1][PIECE_PAWN as usize] == 0
        {
            let white_minor_pieces =
                self.piece_counts[0][PIECE_BISHOP as usize] + self.piece_counts[0][PIECE_KNIGHT as usize];
            let black_minor_pieces =
                self.piece_counts[1][PIECE_BISHOP as usize] + self.piece_counts[1][PIECE_KNIGHT as usize];

            if white_minor_pieces == 1
                && black_minor_pieces == 1
                && self.piece_counts[0][PIECE_BISHOP as usize] == 1
                && self.piece_counts[1][PIECE_BISHOP as usize] == 1
            {
                let bishops = self.piece_bitboards[0][PIECE_BISHOP as usize] | self.piece_bitboards[1][PIECE_BISHOP as usize];
                return (bishops & LIGHT_SQUARES).count_ones() != 1;
            }

            return (white_minor_pieces == 0 && black_minor_pieces == 0)
                || (white_minor_pieces == 0 && black_minor_pieces == 1)
                || (white_minor_pieces == 1 && black_minor_pieces == 0);
        }
        false
    }
}

#[cfg(test)]
mod eval_tests {
    use crate::STARTING_FEN;

    use super::*;

    #[test]
    pub fn simplest_kings_mirrorred() {
        let b1 = Board::from_fen("8/8/8/1k6/8/8/8/4K3 w - - 0 1").unwrap();
        let b2 = Board::from_fen("4k3/8/8/8/1K6/8/8/8 b - - 0 1").unwrap();

        assert_eq!(b1.evaluate(), -b2.evaluate());
        assert_eq!(b1.evaluate_side_to_move_relative(), b2.evaluate_side_to_move_relative());
    }

    #[test]
    pub fn unbalanced_pieces_mirrorred() {
        let b1 = Board::from_fen("4k3/8/8/8/2P5/1PB2N2/6Q1/2R1K3 w - - 0 1").unwrap();
        let b2 = Board::from_fen("2r1k3/6q1/1pb2n2/2p5/8/8/8/4K3 b - - 0 1").unwrap();

        assert_eq!(b1.evaluate(), -b2.evaluate());
        assert_eq!(b1.evaluate_side_to_move_relative(), b2.evaluate_side_to_move_relative());
    }

    #[test]
    pub fn starting_position_is_even() {
        let b = Board::from_fen(STARTING_FEN).unwrap();

        assert_eq!(0, b.evaluate());
    }
}
