use rand::random;

use crate::board::{Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_INVALID, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK};

// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES: [i32; 7] = [0, 100, 300, 350, 500, 900, 20000];

impl Board {
    pub fn evaluate(&self) -> i32 {
        let mut result = 0;

        // white then black
        let mut piece_counts = [[0i8; 7]; 2];
        for i in 0..64 {
            let piece = self.get_piece_64(i);
            if piece != PIECE_NONE && piece != PIECE_INVALID {
                let color = (piece & COLOR_FLAG_MASK == COLOR_BLACK) as usize;
                let piece_type = (piece & PIECE_MASK) as usize;
                piece_counts[color][piece_type] += 1;
            }
        }

        for i in 1..7 {
            result += CENTIPAWN_VALUES[i] * (piece_counts[0][i] - piece_counts[1][i]) as i32;
        }

        // Add a small variance to try to avoid repetition
        result += (random::<i32>() % 11) - 5;

        result
    }

    pub fn evaluate_checkmate(&self) -> i32 {
        if self.white_to_move { -20000 } else { 20000 }
    }

    pub fn evaluate_side_to_move_relative(&self) -> i32 {
        self.evaluate() * if self.white_to_move { 1 } else { -1 }
    }

    pub fn evaluate_checkmate_side_to_move_relative(&self) -> i32 {
        self.evaluate_checkmate() * if self.white_to_move { 1 } else { -1 }
    }

}
