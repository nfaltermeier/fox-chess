use crate::{
    bitboard::{BIT_SQUARES, LIGHT_SQUARES},
    board::{Board, PIECE_BISHOP, PIECE_KNIGHT, PIECE_MASK, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK},
    magic_bitboard::{lookup_bishop_attack, lookup_rook_attack},
    moves::Move,
};

/// 0 is no piece
pub static PIECE_VALUES_SEE: [i16; 7] = [0, 97, 359, 378, 544, 1097, 20000];

pub const MATE_THRESHOLD: i16 = 29500;
pub const MATE_VALUE: i16 = 30000;

impl Board {
    pub fn evaluate_checkmate(&self, ply: u8) -> i16 {
        if self.white_to_move {
            -MATE_VALUE + (ply as i16)
        } else {
            MATE_VALUE - (ply as i16)
        }
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
                let bishops =
                    self.piece_bitboards[0][PIECE_BISHOP as usize] | self.piece_bitboards[1][PIECE_BISHOP as usize];
                return (bishops & LIGHT_SQUARES).count_ones() != 1;
            }

            return (white_minor_pieces == 0 && black_minor_pieces == 0)
                || (white_minor_pieces == 0 && black_minor_pieces == 1)
                || (white_minor_pieces == 1 && black_minor_pieces == 0);
        }
        false
    }

    // Based on the code from https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm
    /// Move is expected to be a capture but probably will work if it isn't. En passant, castling, and promotions are not supported.
    pub fn is_static_exchange_eval_at_least(&self, m: Move, threshold: i16) -> bool {
        let from = m.from() as usize;
        let to = m.to();

        let mut values = [0; 32];
        values[0] = PIECE_VALUES_SEE[(self.get_piece_64(to as usize) & PIECE_MASK) as usize];
        let mut last_attacker = (self.get_piece_64(from) & PIECE_MASK) as usize;

        // If losing the attacker with no followup is greater than the threshold, then no need to investigate further
        if values[0] - PIECE_VALUES_SEE[last_attacker] >= threshold {
            return true;
        }

        let mut occupancy = self.occupancy & !BIT_SQUARES[from];
        let mut attacks_data = self.get_attacks_to(to, occupancy);
        attacks_data.attackers &= !BIT_SQUARES[from];

        let mut depth = 1;
        let mut color = if self.white_to_move { 1 } else { 0 };

        loop {
            // Check if the last move opened up an x-ray
            if attacks_data.possible_rook_like_x_rays != 0
                && (last_attacker == PIECE_ROOK as usize || last_attacker == PIECE_QUEEN as usize)
            {
                let new_attacks = lookup_rook_attack(to, occupancy) & attacks_data.possible_rook_like_x_rays;
                attacks_data.attackers |= new_attacks;
                attacks_data.possible_rook_like_x_rays ^= new_attacks;
            }

            if attacks_data.possible_bishop_like_x_rays != 0
                && (last_attacker == PIECE_BISHOP as usize
                    || last_attacker == PIECE_QUEEN as usize
                    || last_attacker == PIECE_PAWN as usize)
            {
                let new_attacks = lookup_bishop_attack(to, occupancy) & attacks_data.possible_bishop_like_x_rays;
                attacks_data.attackers |= new_attacks;
                attacks_data.possible_bishop_like_x_rays ^= new_attacks;
            }

            let (attacker_bitboard, next_attacker_piece) =
                self.get_least_valuable_attacker(attacks_data.attackers, color);
            if attacker_bitboard == 0 {
                break;
            }

            attacks_data.attackers ^= attacker_bitboard;
            occupancy ^= attacker_bitboard;
            values[depth] = PIECE_VALUES_SEE[last_attacker] - values[depth - 1];

            // If the other side just moved and their value is less than the negative threshold value,
            // then break to ensure that they wouldn't stop earlier
            if depth % 2 == 1 && values[depth].max(-values[depth - 1]) <= -threshold {
                break;
            }

            depth += 1;
            color = if color != 0 { 0 } else { 1 };
            last_attacker = next_attacker_piece;
        }

        for i in (1..depth).rev() {
            values[i - 1] = -values[i].max(-values[i - 1]);
        }

        values[0] >= threshold
    }
}

#[cfg(test)]
mod eval_tests {
    use crate::magic_bitboard::initialize_magic_bitboards;

    use super::*;

    macro_rules! see_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (fen, expected_eval, m_str) = $value;

                    let board = Board::from_fen(fen, None).unwrap();

                    initialize_magic_bitboards();

                    let m = Move::from_simple_long_algebraic_notation(m_str, 0);

                    assert!(board.is_static_exchange_eval_at_least(m, expected_eval - 1));
                    assert!(board.is_static_exchange_eval_at_least(m, expected_eval));
                    assert!(!board.is_static_exchange_eval_at_least(m, expected_eval + 1));
                }
            )*
        }
    }

    see_test! {
        // Some positions taken from https://github.com/zzzzz151/Starzix/blob/main/tests/SEE.txt
        no_recapture: ("1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - -", PIECE_VALUES_SEE[PIECE_PAWN as usize], "e1e5"),
        pawn_captures: ("k7/8/4p1p1/5p2/4P1P1/8/8/K7 w - - 0 1", 0, "e4f5"),
        sliders_behind_capturing_piece: ("2r2r1k/6bp/p7/2q2p1Q/3PpP2/1B6/P5PP/2RR3K b - -", PIECE_VALUES_SEE[PIECE_ROOK as usize] * 2 - PIECE_VALUES_SEE[PIECE_QUEEN as usize], "c5c1"),
        pawn_before_rook: ("4R3/2r3p1/5bk1/1p1r1p1p/p2PR1P1/P1BK1P2/1P6/8 b - -", 0, "h5g4"),
        bishop_for_knight_no_losing_queen_capture: ("5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - -", -PIECE_VALUES_SEE[PIECE_BISHOP as usize] + PIECE_VALUES_SEE[PIECE_KNIGHT as usize], "d6f4"),
        non_capture1: ("2r1k2r/pb4pp/5p1b/2KB3n/4N3/2NP1PB1/PPP1P1PP/R2Q3R w k -", -PIECE_VALUES_SEE[PIECE_BISHOP as usize], "d5c6"),
        non_capture1_recapture: ("2r1k2r/pb4pp/5p1b/2KB3n/1N2N3/3P1PB1/PPP1P1PP/R2Q3R w k -", 0, "d5c6"),
        rook_xray: ("4q3/1p1pr1k1/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -", PIECE_VALUES_SEE[PIECE_PAWN as usize] - PIECE_VALUES_SEE[PIECE_ROOK as usize], "e6e4"),
        rook_xray_extra_defender: ("4q3/1p1pr1kb/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -", PIECE_VALUES_SEE[PIECE_PAWN as usize], "e6e4"),
        // I think the best is if everything gets traded off, this is the net change of that. It fails, not sure if that is because my bishop val != knight val
        // big_trade_both_xrays: ("3r3k/3r4/2n1n3/8/3p4/2PR4/1B1Q4/3R3K w - -", PIECE_VALUES_SEE[PIECE_KNIGHT as usize] * 2 - PIECE_VALUES_SEE[PIECE_BISHOP as usize] + PIECE_VALUES_SEE[PIECE_ROOK as usize] - PIECE_VALUES_SEE[PIECE_QUEEN as usize], "d3d4"),
        bench_is_at_least: ("3r1rk1/ppp1pp1p/6p1/3qb2P/3n4/4BN2/PP2BP2/R2Q1RK1 w - - 0 16", -PIECE_VALUES_SEE[PIECE_QUEEN as usize] + PIECE_VALUES_SEE[PIECE_BISHOP as usize] + PIECE_VALUES_SEE[PIECE_KNIGHT as usize], "d1d4"),
    }
}
