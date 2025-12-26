use array_macro::array;

use crate::{
    bitboard::{BIT_SQUARES, LIGHT_SQUARES, north_fill, south_fill}, board::{
        BISHOP_COLORS_DARK, BISHOP_COLORS_LIGHT, Board, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_PAWN,
        PIECE_QUEEN, PIECE_ROOK,
    }, eval_values::{BISHOP_PAIR, CENTIPAWN_VALUES_ENDGAME, CENTIPAWN_VALUES_MIDGAME, CONNECTED_PAWNS, DOUBLED_PAWN, PASSED_PAWNS, PAWN_SHIELD, ROOF_HALF_OPEN_FILES, ROOK_OPEN_FILES}, magic_bitboard::{lookup_bishop_attack, lookup_rook_attack}, moves::Move
};

/// Indexed with piece code, so index 0 is no piece
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

static FILES: [u64; 8] = array![i => 0x0101010101010101 << i; 8];

impl Board {
    pub fn evaluate(&self) -> i16 {
        let mut midgame_values = self.piecesquare_midgame;
        let mut endgame_values = self.piecesquare_endgame;
        for i in 1..7 {
            midgame_values += CENTIPAWN_VALUES_MIDGAME[i] * (self.piece_counts[0][i] as i16 - self.piece_counts[1][i] as i16);
            endgame_values += CENTIPAWN_VALUES_ENDGAME[i] * (self.piece_counts[0][i] as i16 - self.piece_counts[1][i] as i16);
        }

        let doubled_pawns = self.count_doubled_pawns();
        midgame_values += doubled_pawns * DOUBLED_PAWN.midgame;
        endgame_values += doubled_pawns * DOUBLED_PAWN.endgame;

        let white_passed = self.white_passed_pawns();
        let white_passed_distance = (south_fill(white_passed) & !white_passed).count_ones() as i16;

        let black_passed = self.black_passed_pawns();
        let black_passed_distance = (north_fill(black_passed) & !black_passed).count_ones() as i16;

        let net_passed_pawns = white_passed_distance - black_passed_distance;
        let net_connected_pawns =
            self.get_connected_pawns(true).count_ones() as i16 - self.get_connected_pawns(false).count_ones() as i16;
        midgame_values += net_passed_pawns * PASSED_PAWNS.midgame;
        endgame_values += net_passed_pawns * PASSED_PAWNS.endgame;
        midgame_values += net_connected_pawns * CONNECTED_PAWNS.midgame;
        endgame_values += net_connected_pawns * CONNECTED_PAWNS.endgame;

        let (w_open, w_half_open) = self.rooks_on_open_files(true);
        let (b_open, b_half_open) = self.rooks_on_open_files(false);
        midgame_values += (w_open - b_open) * ROOK_OPEN_FILES.midgame;
        endgame_values += (w_open - b_open) * ROOK_OPEN_FILES.endgame;
        midgame_values += (w_half_open - b_half_open) * ROOF_HALF_OPEN_FILES.midgame;
        endgame_values += (w_half_open - b_half_open) * ROOF_HALF_OPEN_FILES.endgame;

        let bishop_pair = if self.bishop_colors[0] == BISHOP_COLORS_LIGHT | BISHOP_COLORS_DARK
            && self.bishop_colors[1] != BISHOP_COLORS_LIGHT | BISHOP_COLORS_DARK
        {
            1
        } else if self.bishop_colors[0] != BISHOP_COLORS_LIGHT | BISHOP_COLORS_DARK
            && self.bishop_colors[1] == BISHOP_COLORS_LIGHT | BISHOP_COLORS_DARK
        {
            -1
        } else {
            0
        };
        midgame_values += bishop_pair * BISHOP_PAIR.midgame;
        endgame_values += bishop_pair * BISHOP_PAIR.endgame;

        let mut pawn_shield_eval = 0;
        let game_stage_for_pawn_shield = if self.game_stage <= ENDGAME_GAME_STAGE_FOR_QUIESCENSE {
            0
        } else {
            self.game_stage - ENDGAME_GAME_STAGE_FOR_QUIESCENSE
        };
        if game_stage_for_pawn_shield > 0 {
            // How much pawn shield each side is missing. Positive: white is missing more
            let net_pawn_shield_penalty = (6 - self.score_pawn_shield(0)) - (6 - self.score_pawn_shield(1));
            pawn_shield_eval = (game_stage_for_pawn_shield * net_pawn_shield_penalty * PAWN_SHIELD)
                / (MAX_GAME_STAGE - ENDGAME_GAME_STAGE_FOR_QUIESCENSE);
        }

        let mut capped_game_stage = self.game_stage as i32;
        if capped_game_stage > MIN_GAME_STAGE_FULLY_MIDGAME as i32 {
            capped_game_stage = MIN_GAME_STAGE_FULLY_MIDGAME as i32;
        }

        let main_total = (((midgame_values as i32 * capped_game_stage)
            + (endgame_values as i32 * (MIN_GAME_STAGE_FULLY_MIDGAME as i32 - capped_game_stage)))
            / (MIN_GAME_STAGE_FULLY_MIDGAME as i32)) as i16;

        main_total + pawn_shield_eval
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

    /// positive value: black has more doubled pawns than white
    fn count_doubled_pawns(&self) -> i16 {
        let mut pawn_occupied_files = [0, 0];
        for (color, occupied_files_count) in pawn_occupied_files.iter_mut().enumerate() {
            for file in FILES {
                if self.piece_bitboards[color][PIECE_PAWN as usize] & file > 0 {
                    *occupied_files_count += 1;
                }
            }
        }

        (self.piece_counts[1][PIECE_PAWN as usize] as i16 - pawn_occupied_files[1])
            - (self.piece_counts[0][PIECE_PAWN as usize] as i16 - pawn_occupied_files[0])
    }

    // Algorithm from https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm
    /// Move is expected to be a capture but probably will work if it isn't. En passant, castling, and promotions are not supported.
    pub fn static_exchange_eval(&self, m: Move) -> i16 {
        let from = m.from();
        let to = m.to();
        let mut occupancy = self.occupancy & !BIT_SQUARES[from as usize];
        let mut attacks_data = self.get_attacks_to(to as u8, occupancy);
        attacks_data.attackers &= !BIT_SQUARES[from as usize];

        let mut values = [0; 32];
        let mut depth = 1;
        let mut color = if self.white_to_move { 1 } else { 0 };
        values[0] = CENTIPAWN_VALUES_MIDGAME[(self.get_piece_64(to as usize) & PIECE_MASK) as usize];
        let mut last_attacker = (self.get_piece_64(from as usize) & PIECE_MASK) as usize;

        loop {
            // Check if the last move opened up an x-ray
            if (last_attacker == PIECE_ROOK as usize || last_attacker == PIECE_QUEEN as usize)
                && attacks_data.possible_rook_like_x_rays != 0
            {
                let new_attacks = lookup_rook_attack(to as u8, occupancy) & attacks_data.possible_rook_like_x_rays;
                attacks_data.attackers |= new_attacks;
                attacks_data.possible_rook_like_x_rays ^= new_attacks;
            }

            if (last_attacker == PIECE_BISHOP as usize
                || last_attacker == PIECE_QUEEN as usize
                || last_attacker == PIECE_PAWN as usize)
                && attacks_data.possible_bishop_like_x_rays != 0
            {
                let new_attacks = lookup_bishop_attack(to as u8, occupancy) & attacks_data.possible_bishop_like_x_rays;
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
            values[depth] = CENTIPAWN_VALUES_MIDGAME[last_attacker] - values[depth - 1];
            depth += 1;
            color = if color != 0 { 0 } else { 1 };
            last_attacker = next_attacker_piece;
        }

        for i in (1..depth).rev() {
            values[i - 1] = -values[i].max(-values[i - 1]);
        }

        values[0]
    }

    // Based on the code from https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm
    /// Move is expected to be a capture but probably will work if it isn't. En passant, castling, and promotions are not supported.
    pub fn is_static_exchange_eval_at_least(&self, m: Move, threshold: i16) -> bool {
        let from = m.from();
        let to = m.to();
        let mut occupancy = self.occupancy & !BIT_SQUARES[from as usize];
        let mut attacks_data = self.get_attacks_to(to as u8, occupancy);
        attacks_data.attackers &= !BIT_SQUARES[from as usize];

        let mut values = [0; 32];
        let mut depth = 1;
        let mut color = if self.white_to_move { 1 } else { 0 };
        values[0] = CENTIPAWN_VALUES_MIDGAME[(self.get_piece_64(to as usize) & PIECE_MASK) as usize];
        let mut last_attacker = (self.get_piece_64(from as usize) & PIECE_MASK) as usize;

        loop {
            // Check if the last move opened up an x-ray
            if attacks_data.possible_rook_like_x_rays != 0 &&
                (last_attacker == PIECE_ROOK as usize || last_attacker == PIECE_QUEEN as usize)
            {
                let new_attacks = lookup_rook_attack(to as u8, occupancy) & attacks_data.possible_rook_like_x_rays;
                attacks_data.attackers |= new_attacks;
                attacks_data.possible_rook_like_x_rays ^= new_attacks;
            }

            if attacks_data.possible_bishop_like_x_rays != 0 &&
                (last_attacker == PIECE_BISHOP as usize
                    || last_attacker == PIECE_QUEEN as usize
                    || last_attacker == PIECE_PAWN as usize)
            {
                let new_attacks = lookup_bishop_attack(to as u8, occupancy) & attacks_data.possible_bishop_like_x_rays;
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
            values[depth] = CENTIPAWN_VALUES_MIDGAME[last_attacker] - values[depth - 1];

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
    use crate::STARTING_FEN;
    use crate::magic_bitboard::initialize_magic_bitboards;

    use super::*;

    macro_rules! doubled_pawns_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;

                    let board = Board::from_fen(input).unwrap();
                    let doubled_pawns = board.count_doubled_pawns();

                    assert_eq!(expected, doubled_pawns);
                }
            )*
        }
    }

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

    doubled_pawns_test! {
        starting_position: (STARTING_FEN, 0),
        white_two_doubled: ("rnbqkbnr/pppppppp/8/8/8/1P4P1/PP1PP1PP/RNBQKBNR w KQkq - 0 1", -2),
        black_two_doubled: ("rnbqkbnr/1ppp1ppp/1p5p/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 2),
        white_tripled: ("rnbqkbnr/pppppppp/8/8/3P4/3P4/PP1P1PPP/RNBQKBNR w KQkq - 0 1", -2),
        black_tripled: ("rnbqkbnr/1ppp1ppp/1p6/1p6/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 2),
        white_only_single_pawn: ("1k6/8/8/8/8/8/4P3/4K3 w - - 0 1", 0),
        white_only_doubled_pawn: ("1k6/8/8/8/8/4P3/4P3/4K3 w - - 0 1", -1),
        black_only_single_pawn: ("1k6/1p6/8/8/8/8/8/4K3 w - - 0 1", 0),
        black_only_doubled_pawn: ("1k6/1p6/1p6/8/8/8/8/4K3 w - - 0 1", 1),
        unbalanced: ("1k6/1p2pp2/1p6/8/8/4P1P1/4P1P1/4K3 w - - 0 1", -1),
        unbalanced_opposite_colors: ("1K6/1P2PP2/1P6/8/8/4p1p1/4p1p1/4k3 w - - 0 1", 1),
    }

    macro_rules! see_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (fen, expected_eval, m_str) = $value;

                    let board = Board::from_fen(fen).unwrap();

                    initialize_magic_bitboards();

                    let m = Move::from_simple_long_algebraic_notation(m_str, 0);
                    let see_result = board.static_exchange_eval(m);

                    assert_eq!(expected_eval, see_result);
                    assert!(board.is_static_exchange_eval_at_least(m, expected_eval - 1));
                    assert!(board.is_static_exchange_eval_at_least(m, expected_eval));
                    assert!(!board.is_static_exchange_eval_at_least(m, expected_eval + 1));
                }
            )*
        }
    }

    see_test! {
        // Some positions taken from https://github.com/zzzzz151/Starzix/blob/main/tests/SEE.txt
        no_recapture: ("1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - -", CENTIPAWN_VALUES_MIDGAME[PIECE_PAWN as usize], "e1e5"),
        pawn_captures: ("k7/8/4p1p1/5p2/4P1P1/8/8/K7 w - - 0 1", 0, "e4f5"),
        sliders_behind_capturing_piece: ("2r2r1k/6bp/p7/2q2p1Q/3PpP2/1B6/P5PP/2RR3K b - -", CENTIPAWN_VALUES_MIDGAME[PIECE_ROOK as usize] * 2 - CENTIPAWN_VALUES_MIDGAME[PIECE_QUEEN as usize], "c5c1"),
        pawn_before_rook: ("4R3/2r3p1/5bk1/1p1r1p1p/p2PR1P1/P1BK1P2/1P6/8 b - -", 0, "h5g4"),
        bishop_for_knight_no_losing_queen_capture: ("5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - -", -CENTIPAWN_VALUES_MIDGAME[PIECE_BISHOP as usize] + CENTIPAWN_VALUES_MIDGAME[PIECE_KNIGHT as usize], "d6f4"),
        non_capture1: ("2r1k2r/pb4pp/5p1b/2KB3n/4N3/2NP1PB1/PPP1P1PP/R2Q3R w k -", -CENTIPAWN_VALUES_MIDGAME[PIECE_BISHOP as usize], "d5c6"),
        non_capture1_recapture: ("2r1k2r/pb4pp/5p1b/2KB3n/1N2N3/3P1PB1/PPP1P1PP/R2Q3R w k -", 0, "d5c6"),
        rook_xray: ("4q3/1p1pr1k1/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -", CENTIPAWN_VALUES_MIDGAME[PIECE_PAWN as usize] - CENTIPAWN_VALUES_MIDGAME[PIECE_ROOK as usize], "e6e4"),
        rook_xray_extra_defender: ("4q3/1p1pr1kb/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -", CENTIPAWN_VALUES_MIDGAME[PIECE_PAWN as usize], "e6e4"),
        // I think the best is if everything gets traded off, this is the net change of that. It fails, not sure if that is because my bishop val != knight val
        // big_trade_both_xrays: ("3r3k/3r4/2n1n3/8/3p4/2PR4/1B1Q4/3R3K w - -", CENTIPAWN_VALUES_MIDGAME[PIECE_KNIGHT as usize] * 2 - CENTIPAWN_VALUES_MIDGAME[PIECE_BISHOP as usize] + CENTIPAWN_VALUES_MIDGAME[PIECE_ROOK as usize] - CENTIPAWN_VALUES_MIDGAME[PIECE_QUEEN as usize], "d3d4"),
        bench_is_at_least: ("3r1rk1/ppp1pp1p/6p1/3qb2P/3n4/4BN2/PP2BP2/R2Q1RK1 w - - 0 16", -298, "d1d4"),
    }
}
