use std::cell::Cell;

use array_macro::array;
use rand::random;

use crate::board::{
    file_8x8, Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_INVALID, PIECE_KING, PIECE_KNIGHT, PIECE_MASK,
    PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK,
};

// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES: [i16; 7] = [0, 100, 315, 350, 500, 900, 20000];

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

#[rustfmt::skip]
// piece square table values are taken from https://www.chessprogramming.org/Simplified_Evaluation_Function
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
    0,  0,  0,  0,  0,  0,  0,  0,
    50, 50, 50, 50, 50, 50, 50, 50,
    10, 10, 20, 30, 30, 20, 10, 10,
     5,  5, 10, 25, 25, 10,  5,  5,
     0,  0,  0, 20, 20,  0,  0,  0,
     5, -5,-10,  0,  0,-10, -5,  5,
     5, 10, 10,-20,-20, 10, 10,  5,
     0,  0,  0,  0,  0,  0,  0,  0
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
    0,  0,  0,  0,  0,  0,  0,  0,
    50, 50, 50, 50, 50, 50, 50, 50,
    30, 30, 30, 30, 30, 30, 30, 30,
    20, 20, 20, 20, 20, 20, 20, 20,
    10, 10, 10, 10, 10, 10, 10, 10,
     0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0
];

#[rustfmt::skip]
const KNIGHT_SQUARE_TABLE: [i16; 64] = [
    -50,-40,-30,-30,-30,-30,-40,-50,
    -40,-20,  0,  0,  0,  0,-20,-40,
    -30,  0, 10, 15, 15, 10,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -40,-20,  0,  5,  5,  0,-20,-40,
    -50,-40,-30,-30,-30,-30,-40,-50,
];

#[rustfmt::skip]
const BISHOP_SQUARE_TABLE: [i16; 64] = [
    -20,-10,-10,-10,-10,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5, 10, 10,  5,  0,-10,
    -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0, 10, 10, 10, 10,  0,-10,
    -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  5,  0,  0,  0,  0,  5,-10,
    -20,-10,-10,-10,-10,-10,-10,-20,
];

#[rustfmt::skip]
const ROOK_SQUARE_TABLE: [i16; 64] = [
    0,  0,  0,  0,  0,  0,  0,  0,
    5, 10, 10, 10, 10, 10, 10,  5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
   -5,  0,  0,  0,  0,  0,  0, -5,
    0,  0,  0,  5,  5,  0,  0,  0
];

#[rustfmt::skip]
const QUEEN_SQUARE_TABLE: [i16; 64] = [
    -20,-10,-10, -5, -5,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5,  5,  5,  5,  0,-10,
     -5,  0,  5,  5,  5,  5,  0, -5,
      0,  0,  5,  5,  5,  5,  0, -5,
    -10,  5,  5,  5,  5,  5,  0,-10,
    -10,  0,  5,  0,  0,  0,  0,-10,
    -20,-10,-10, -5, -5,-10,-10,-20
];

#[rustfmt::skip]
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -20,-30,-30,-40,-40,-30,-30,-20,
    -10,-20,-20,-20,-20,-20,-20,-10,
    20, 20,  0,  0,  0,  0, 20, 20,
    20, 30, 10,  0,  0, 10, 30, 20
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
    -50,-40,-30,-20,-20,-30,-40,-50,
    -30,-20,-10,  0,  0,-10,-20,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-30,  0,  0,  0,  0,-30,-30,
    -50,-30,-30,-30,-30,-30,-30,-50
];

const ALL_PIECE_SQUARE_TABLES: [[i16; 64]; 12] = [
    PAWN_MIDGAME_SQUARE_TABLE,
    KNIGHT_SQUARE_TABLE,
    BISHOP_SQUARE_TABLE,
    ROOK_SQUARE_TABLE,
    QUEEN_SQUARE_TABLE,
    KING_MIDGAME_SQUARE_TABLE,
    PAWN_ENDGAME_SQUARE_TABLE,
    KNIGHT_SQUARE_TABLE,
    BISHOP_SQUARE_TABLE,
    ROOK_SQUARE_TABLE,
    QUEEN_SQUARE_TABLE,
    KING_ENDGAME_SQUARE_TABLE,
];

static PIECE_SQUARE_TABLES: [[[i16; 64]; 12]; 2] = [
    // vertically flip each table for white
    array![x => array![y => ALL_PIECE_SQUARE_TABLES[x][y ^ 0b00111000]; 64]; 12],
    // Evaluate from white's perspective so negate each score for black
    array![x => array![y => -ALL_PIECE_SQUARE_TABLES[x][y]; 64]; 12],
];

thread_local! {
    pub static ISOLATED_PAWN_PENALTY: Cell<i16> = const { Cell::new(35) };
    pub static DOUBLED_PAWN_PENALTY: Cell<i16> = const { Cell::new(25) };
}

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
            if piece != PIECE_NONE && piece != PIECE_INVALID {
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
        material_score + position_score_final + (random::<i16>() % 11) - 5 + doubled_pawns * DOUBLED_PAWN_PENALTY.get()
    }

    pub fn evaluate_checkmate(&self, ply: u8) -> i16 {
        if self.white_to_move {
            -20000 + (ply as i16) * 10
        } else {
            20000 - (ply as i16) * 10
        }
    }

    pub fn evaluate_side_to_move_relative(&self) -> i16 {
        self.evaluate() * if self.white_to_move { 1 } else { -1 }
    }

    pub fn evaluate_checkmate_side_to_move_relative(&self, ply: u8) -> i16 {
        self.evaluate_checkmate(ply) * if self.white_to_move { 1 } else { -1 }
    }
}
