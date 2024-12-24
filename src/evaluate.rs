use rand::random;

use crate::board::{Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_INVALID, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK};

impl Board {
    pub fn evaluate(&self) -> i32 {
        let mut result = 0;

        // white then black
        let mut piece_counts = [[0i8; 6]; 2];
        for i in 0..64 {
            let piece = self.get_piece_64(i);
            if piece != PIECE_NONE && piece != PIECE_INVALID {
                let color = (piece & COLOR_FLAG_MASK == COLOR_BLACK) as usize;
                let piece_type = (piece & PIECE_MASK) as usize;
                piece_counts[color][piece_type - 1] += 1;
            }
        }

        result += 20000 * ((piece_counts[0][PIECE_KING as usize - 1] - piece_counts[1][PIECE_KING as usize - 1]) as i32);
        result += 900 * ((piece_counts[0][PIECE_QUEEN as usize - 1] - piece_counts[1][PIECE_QUEEN as usize - 1]) as i32);
        result += 500 * ((piece_counts[0][PIECE_ROOK as usize - 1] - piece_counts[1][PIECE_ROOK as usize - 1]) as i32);
        result += 350 * ((piece_counts[0][PIECE_BISHOP as usize - 1] - piece_counts[1][PIECE_BISHOP as usize - 1]) as i32);
        result += 300 * ((piece_counts[0][PIECE_KNIGHT as usize - 1] - piece_counts[1][PIECE_KNIGHT as usize - 1]) as i32);
        result += 100 * ((piece_counts[0][PIECE_PAWN as usize - 1] - piece_counts[1][PIECE_PAWN as usize - 1]) as i32);

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
