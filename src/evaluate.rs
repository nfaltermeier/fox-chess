use std::cell::Cell;

use array_macro::array;
use rand::random;

use crate::board::{
    file_8x8, Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_MASK,
    PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK,
};

// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES: [i16; 7] = [0, 87, 309, 338, 502, 1021, 20000];

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
 178, 237, 186, 150, 129, 107,  63, 130,
  45,  63,  62,  84,  94,  63,  94,  42,
  -1,   6,  12,  26,  20,  16,   0,  -3,
 -10,   6,   0,   9,   9,   2,   0, -16,
 -17,   9,   8,   2,   1,   9,  22,  -1,
 -25,  -1, -11,  -4,  -4,  11,  28, -19,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 253, 269, 287, 187, 191, 262, 276, 254,
 136, 166, 162, 124, 104,  67, 116,  98,
  75,  83,  58,  23,   3,  13,  52,  32,
  44,  35,  24,   3,   5,  18,  32,  17,
  39,  42,  20,  45,  28,  27,  16,   6,
  49,  47,  32,  41,   7,  20,   4,   8,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-152, -29, -14, -13,  31, -42, -40, -87,
 -38,  -7,  18,  54,  39,  41, -25,  -8,
  -4,  31,  38,  53,  82,  76,  42,  15,
  14,   2,  22,  49,  17,  49,  13,  43,
   1,  -2,  17,   9,  23,  21,  23,   5,
  -5,  -7,   4,  14,  24,   1,  15, -14,
 -55, -15,  -2,  -4,  -4,   1, -24,   5,
 -76, -10, -35, -34, -22,  -9, -14, -50,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -50, -38, -28, -37, -33, -30, -40, -53,
 -42, -22, -63, -31, -26, -67, -21, -65,
 -13,  -5, -37, -56, -94, -53,  -2, -64,
 -71, -31, -42, -53, -19, -49, -37, -76,
 -59, -41, -15, -15, -11, -38, -21, -39,
 -76, -53, -42, -46, -39, -38, -64, -63,
 -44, -20, -35, -56, -48, -68, -33,-146,
-105, -84, -43, -71, -84, -55, -40, -50,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -29, -33, -30, -29, -10, -46,   0, -19,
 -30, -24,   9,  10,   6,  -3,   4, -34,
 -11,   1,  20,  32,  40,  70,  24,  21,
  14,  -3,  12,  27,  26,  10,  -4,   8,
 -12,   8,   1,  17,  18,   9,   6,   0,
  15,   6,  12,   4,   7,   2,   2,  18,
  15,  -9,  10,  -5,  -2,  -2,   9, -29,
 -53, -19, -20, -14, -15, -17,   0, -18,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -20, -22, -18, -10, -10, -10, -10, -20,
  -1,  -6, -44, -39, -12, -33, -26, -37,
 -24,   7, -32, -60, -61, -39,  -5, -56,
 -39, -15, -32, -37, -19, -18, -16, -46,
 -33, -18,  -8, -13, -30, -32, -35, -79,
 -82, -34,  -6, -37, -17, -28, -87, -92,
-101, -61, -91, -38, -62, -51, -94,-173,
 -36, -25, -85, -58, -43, -68, -56, -39,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  36,  61,  53,  48,  75,  80,  84,  15,
  39,  38,  66,  73,  66,  93,  76,  56,
  26,  67,  60,  46,  46,  70,  73,  52,
  18,  15,  21,  25,  44,  41,  42,  22,
  -9,   8,   8,  20,  12,   8,  29,   6,
  -7,  11,   3,   2,   0,   6,  33,  24,
 -23,  -9,   0,   1,  -3,   6,   7, -36,
   0,   1,  12,  11,  10,   6,  -3,   0,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  34,  12,  16,  30,  -3,   1,  17,  37,
  27,  36,  10,  19,  14, -11,  10,  -1,
  35,  -4,  26,  22,  19,  -1,  10,  -2,
  24,  39,  40,  33,   7,  12,  24,   8,
  39,  21,  44,  21,  31,  31,   1,   7,
   9,   0,  16,  22,  19,  14, -40, -19,
  10,   6,  27,   1,   8, -22, -25,  -5,
   8,   4,  15,  29,   1,  -3,  12,  14,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  -3,  13,  39,  47,  54,  77,  96,  31,
 -21, -35,   8,  16,  24,  75,  30,  65,
  -1,   2,   2,  19,  42,  97,  75,  76,
  -2,  -9,   1,  22,  33,  29,  48,  31,
  -7,  -5,  -2,   9,   8,  13,  24,  18,
 -11,   1,   4,   2,   6,  14,  17,   1,
 -14,  -5,   2,   0,   9,   7,  -6,  -3,
   0, -15,  -9,   0,  -9, -21, -30, -54,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  15,  71,  53,  70,  -2,   7,  -5, -20,
  44, 114, 115,  96, 142,  89,  74,   1,
  12,   1,  82,  74, 106,  50,   1,  -9,
  69,  60,  86,  72, 121, 107,  50,  21,
  31,  54,  79,  67, 110,  98,  16,  -5,
  -8,  34,  89,  45,  32,  70,   7, -10,
 -10,   0,  45,   8,  11, -26, -44, -10,
 -20, -27, -10,   9,  -5, -11, -10, -20,
];

#[rustfmt::skip]
/// This appears to have some anomalous values
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -30, -40, -40, -50, -50, -40, -40, -30,
 -30, -30, -39, -50, -50, -40,  -2, -30,
 -30,  31, -14, -24,   0,   0,   0,   0,
 -30,   0,   0,   0,   0,   0,   0,   0,
 -20,   0,   0,   0,   0,   0, -20,   0,
 -13,   0,   0,   0,  -5,  -4,   2,  -9,
   0,   0,   0, -21, -13,  -7,   0,   0,
  -8,  42,  25, -24,  39, -26,  26,  19,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -50, -36, -30, -20, -20,   0, -20, -50,
 -30,  46,   2,  42,  44,  69,  78, -16,
  -4,  24,  35,  37,  37,  19,  17,  41,
 -30,  -1,  30,  22,  13,  -9, -19,  -1,
 -30,  -3,   8,   9,  18,  25,  16,   7,
  -5, -16,   0,   6,  21,  20,   8,   4,
 -32,  -8,   0,  20,  10,   8,  -6, -23,
 -34, -62, -26, -25, -73, -10, -54, -60,
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

        // Add a small variance to try to avoid repetition
        // isolated_pawns * ISOLATED_PAWN_PENALTY.get()
        material_score + position_score_final + doubled_pawns * 17
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

            // TODO: Does not account for bishop vs bishop of same color. Should be simple to check with bitboards.
            return (white_minor_pieces == 0 && black_minor_pieces == 0)
                || (white_minor_pieces == 0 && black_minor_pieces == 1)
                || (white_minor_pieces == 1 && black_minor_pieces == 0);
        }
        false
    }
}

#[cfg(test)]
mod eval_tests {
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
}
