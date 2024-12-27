use array_macro::array;
use rand::random;

use crate::board::{
    Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_INVALID, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_NONE,
    PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK,
};

// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES: [i32; 7] = [0, 100, 315, 350, 500, 900, 20000];

static GAME_STAGE_VALUES: [i32; 7] = [0, 0, 4, 4, 4, 8, 0];
const MAX_GAME_STAGE: i32 = 64;
const GAME_STAGE_FULLY_ENDGAME: i32 = 16;

#[rustfmt::skip]
// piece square table values are taken from https://www.chessprogramming.org/Simplified_Evaluation_Function
const PAWN_SQUARE_TABLE: [i32; 64] = [
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
const KNIGHT_SQUARE_TABLE: [i32; 64] = [
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
const BISHOP_SQUARE_TABLE: [i32; 64] = [
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
const ROOK_SQUARE_TABLE: [i32; 64] = [
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
const QUEEN_SQUARE_TABLE: [i32; 64] = [
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
const KING_MIDGAME_SQUARE_TABLE: [i32; 64] = [
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
const KING_ENDGAME_SQUARE_TABLE: [i32; 64] = [
    -50,-40,-30,-20,-20,-30,-40,-50,
    -30,-20,-10,  0,  0,-10,-20,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-30,  0,  0,  0,  0,-30,-30,
    -50,-30,-30,-30,-30,-30,-30,-50
];

const ALL_PIECE_SQUARE_TABLES: [[i32; 64]; 7] = [ PAWN_SQUARE_TABLE, KNIGHT_SQUARE_TABLE, BISHOP_SQUARE_TABLE, ROOK_SQUARE_TABLE, QUEEN_SQUARE_TABLE, KING_MIDGAME_SQUARE_TABLE, KING_ENDGAME_SQUARE_TABLE ];

static PIECE_SQUARE_TABLES: [[[i32; 64]; 7]; 2] = [
    // vertically flip each table for white
    array![x => array![y => ALL_PIECE_SQUARE_TABLES[x][y ^ 0b00111000]; 64]; 7],
    // Evaluate from white's perspective so negate each score for black
    array![x => array![y => -ALL_PIECE_SQUARE_TABLES[x][y]; 64]; 7],
];

impl Board {
    pub fn evaluate(&self) -> i32 {
        let mut material_score = 0;
        let mut position_score = 0;
        let mut game_stage = 0;
        let mut king_pos = [0, 0];

        // white then black
        let mut piece_counts = [[0i8; 7]; 2];
        for i in 0..64 {
            let piece = self.get_piece_64(i);
            if piece != PIECE_NONE && piece != PIECE_INVALID {
                let color = (piece & COLOR_FLAG_MASK == COLOR_BLACK) as usize;
                let piece_type = (piece & PIECE_MASK) as usize;
                piece_counts[color][piece_type] += 1;

                // todo endgame vs midgame
                if piece_type != PIECE_KING as usize {
                    position_score += PIECE_SQUARE_TABLES[color][piece_type - 1][i];
                } else {
                    king_pos[color] = i;
                }
            }
        }

        for i in 1..7 {
            material_score += CENTIPAWN_VALUES[i] * (piece_counts[0][i] - piece_counts[1][i]) as i32;

            game_stage += GAME_STAGE_VALUES[i] * (piece_counts[0][i] + piece_counts[1][i]) as i32;
        }

        if game_stage > MAX_GAME_STAGE {
            game_stage = MAX_GAME_STAGE;
        }

        for i in 0..=1 {
            position_score += (
                    (PIECE_SQUARE_TABLES[i][PIECE_KING as usize - 1][king_pos[i]] * game_stage)
                    + (PIECE_SQUARE_TABLES[i][PIECE_KING as usize][king_pos[i]] * (MAX_GAME_STAGE - game_stage))
                ) / (MAX_GAME_STAGE);
        }

        // Add a small variance to try to avoid repetition
        material_score + position_score + (random::<i32>() % 11) - 5
    }

    pub fn evaluate_checkmate(&self) -> i32 {
        if self.white_to_move {
            -20000
        } else {
            20000
        }
    }

    pub fn evaluate_side_to_move_relative(&self) -> i32 {
        self.evaluate() * if self.white_to_move { 1 } else { -1 }
    }

    pub fn evaluate_checkmate_side_to_move_relative(&self) -> i32 {
        self.evaluate_checkmate() * if self.white_to_move { 1 } else { -1 }
    }
}
