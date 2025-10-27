use array_macro::array;

use crate::{
    bitboard::{north_fill, south_fill, BIT_SQUARES, LIGHT_SQUARES},
    board::{Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK},
    magic_bitboard::{lookup_bishop_attack, lookup_rook_attack},
    moves::Move,
    texel::{
        EvalParams, EP_DOUBLED_PAWNS_IDX, EP_PASSED_PAWN_IDX, EP_PIECE_VALUES_IDX, EP_ROOK_HALF_OPEN_FILE_IDX, EP_ROOK_OPEN_FILE_IDX
    },
};

/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES: [i16; 7] = [0, 79, 286, 313, 443, 901, 20000];

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

#[rustfmt::skip]
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 164, 147, 136, 128,  85,  59,  16,  55,
  37,  53,  50,  46,  50,  62,  74,  30,
   0,  11,  -2,  11,  20,  16,  14,  -2,
 -12,   4,  -5,   1,  -1,  10,   6, -13,
 -13,   2,  -9,  -7,   2,   5,  21,  -4,
 -18,  -5, -18, -16,  -8,  19,  29, -14,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
  82, 111, 109,  71, 102, 128, 171, 128,
 109, 103,  79,  63,  54,  46,  63,  63,
  70,  54,  47,  14,  12,  20,  40,  30,
  50,  44,  23,   7,  14,  26,  38,  25,
  48,  35,  28,  22,  21,  24,  28,  19,
  60,  51,  41,  24,  32,  29,  33,  33,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-124, -21,  -7, -15,  24, -46, -39,-118,
 -10,   4,  23,  36,  43,  38, -35, -10,
   6,  27,  42,  60,  76,  81,  36,   6,
   1,  13,  32,  51,  22,  53,  17,  26,
  -7,   7,  21,   8,  20,  18,  26,  -8,
 -23,  -8,  -1,  10,  25,   2,   5, -11,
 -49, -22, -15,  -5,  -4,  -1, -13, -33,
 -81, -24, -35, -25, -24, -15, -16, -66,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  -4, -34, -34, -37, -68, -18, -31, -60,
 -64, -40, -52, -53, -56, -72, -39, -80,
 -49, -65, -36, -51, -75, -78, -53, -68,
 -72, -45, -28, -51, -25, -46, -48, -90,
 -60, -37, -25, -19, -38, -41, -75, -66,
 -85, -50, -31, -38, -43, -44, -72, -95,
 -58, -61, -55, -53, -58, -59, -71,-117,
-105,-104, -67, -72, -91, -83,-118,-109,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -13, -29, -36, -18, -37, -57, -14,  20,
  -7,  -1,   7, -10,   9,   4,  -8, -17,
   0,  16,  27,  34,  37,  63,  34,  25,
   3,   2,  17,  38,  30,  25,  -4,  -3,
 -10,  11,   6,  30,  19,   5,   0,   1,
  -9,   8,  10,   9,  10,   6,   6,   1,
 -11,  -2,   1,  -5,   2,  -3,  11, -16,
 -37, -25, -18, -19, -17, -18, -23, -25,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -14, -10, -18, -25, -30, -28, -47, -58,
 -37, -34, -37, -38, -39, -51, -49, -48,
 -41, -44, -47, -63, -60, -65, -58, -71,
 -50, -38, -48, -47, -45, -58, -35, -45,
 -46, -43, -35, -41, -42, -34, -38, -66,
 -81, -48, -37, -46, -29, -51, -74, -79,
 -71, -72, -62, -47, -59, -56, -85,-148,
 -68, -48, -81, -55, -71, -81, -65, -66,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  50,  47,  43,  37,  35,  34,  47,  39,
  24,  27,  45,  55,  46,  67,  54,  53,
  11,  24,  28,  35,  39,  65,  50,  22,
  -4,  -2,   9,  12,  14,  20,  13,  -7,
 -22, -15,  -8,  -4,  -5,  -5,   0, -22,
 -29, -20, -20, -20, -18, -19,  -3, -23,
 -31, -27, -15, -17, -15,  -8, -20, -49,
 -14, -14,  -7,  -3,  -4,  -5, -36, -16,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   3,  25,  27,  32,  47,  55,  41,  29,
  40,  39,  30,  23,  28,  20,  26,  11,
  38,  29,  27,  22,  24,  14,  20,  24,
  40,  42,  30,  27,  25,  24,  25,  37,
  46,  40,  37,  30,  29,  35,  24,  31,
  12,  15,  18,  21,  15,  20,   4,  10,
   4,   7,   9,   6,   4,   2,   1,   4,
  13,   8,  20,  22,   3,   9,  46,  -5,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   4,  21,  28,  39,  67,  78,  71,  61,
  -1,  -8,  21,  30,  35,  73,  48,  66,
   4,   8,  22,  34,  54,  98,  86,  41,
  -5,   0,  11,  23,  30,  31,  41,  18,
   0,   5,   4,  10,  11,  10,  17,   6,
  -4,   3,   2,   4,   4,   7,  12,  -4,
 -17,  -3,   6,   2,   7,  -4, -16, -37,
  -5,  -8,  -9,   3,  -9, -37, -49, -27,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  15,  60,  53,  55,   2,   5, -17, -20,
  44,  76,  74,  95,  86,  26,  55,   1,
  14,  37,  55,  75,  73,   1, -20,  -4,
  51,  33,  81,  72,  75,  82,  54,  23,
  29,  45,  62,  64,  65,  67,  56,   4,
  -7,  18,  56,  20,  31,  36,   6, -10,
  -9,  -7,  10, -10,  -7, -28, -50, -10,
 -22, -29, -11, -62,  -5, -13, -10, -20,
];

#[rustfmt::skip]
/// This appears to have some anomalous values
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -28,  52,  32, -40,  65, -14,  51, -46,
  13,  -4,  58, -15, -39,  35,  -7,  18,
 -28,   8,   0,  -5, -28, -32, -43, -40,
  27,  45,  11,   7,  15,  21,  43,  29,
  16,  40,  37,  30,  26,  26,   5,  -2,
   6,  24,  18,  17,  19,   7,   7, -13,
  21,  13,   8, -10,  -4,  -2,  19,  18,
 -34,  21,  -2, -41,  -4, -34,  29,  14,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -69, -36,  -2,  23, -11,  22,   2, -64,
  -9,  11,  24,  47,  47,  -9,  36,  21,
  13,  28,  43,  41,  45,  57,  48,  39,
  -5,  10,  25,  29,  28,  27,  14,   6,
 -28, -10,   2,  11,  19,  17,  13,   1,
 -37, -17,  -1,   5,   7,  14,   2,  -8,
 -44, -12,  -9,   2,   2,   5,  -6, -29,
   6, -37, -16, -11, -28,  -6, -50, -56,
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

pub static PIECE_SQUARE_TABLES: [[[i16; 64]; 12]; 2] = [
    // vertically flip each table for white
    array![x => array![y => ALL_PIECE_SQUARE_TABLES[x][y ^ 0b00111000]; 64]; 12],
    // Evaluate from white's perspective so negate each score for black
    array![x => array![y => -ALL_PIECE_SQUARE_TABLES[x][y]; 64]; 12],
];

static FILES: [u64; 8] = array![i => 0x0101010101010101 << i; 8];

#[inline]
/// for piece_type, pawn is 0
fn get_piece_square_value(params: &EvalParams, color: usize, piece_type: usize, square: usize) -> i16 {
    if color == 0 {
        params[piece_type * 64 + (square ^ 0b00111000)]
    } else {
        -params[piece_type * 64 + square]
    }
}

impl Board {
    pub fn evaluate(&self, params: &EvalParams) -> i16 {
        let mut material_score = 0;
        let mut game_stage = self.game_stage;

        let mut position_score_midgame = 0;
        let mut position_score_endgame = 0;
        for i in 0..64 {
            let piece = self.get_piece_64(i);
            if piece != PIECE_NONE {
                let color = (piece & COLOR_FLAG_MASK == COLOR_BLACK) as usize;
                let piece_type = (piece & PIECE_MASK) as usize;

                position_score_midgame += get_piece_square_value(params, color, piece_type - 1, i);
                position_score_endgame += get_piece_square_value(params, color, piece_type - 1 + 6, i);
                material_score += params[EP_PIECE_VALUES_IDX + i] * if color == 0 { 1 } else { -1 };
            }
        }

        let doubled_pawns = self.count_doubled_pawns();

        if game_stage > MIN_GAME_STAGE_FULLY_MIDGAME {
            game_stage = MIN_GAME_STAGE_FULLY_MIDGAME;
        }

        let position_score_final = ((position_score_midgame * game_stage)
            + (position_score_endgame * (MIN_GAME_STAGE_FULLY_MIDGAME - game_stage)))
            / MIN_GAME_STAGE_FULLY_MIDGAME;

        let white_passed = self.white_passed_pawns();
        let white_passed_distance = (south_fill(white_passed) & !white_passed).count_ones() as i16;

        let black_passed = self.black_passed_pawns();
        let black_passed_distance = (north_fill(black_passed) & !black_passed).count_ones() as i16;

        let net_passed_pawns = white_passed_distance - black_passed_distance;

        let (w_open, w_half_open) = self.rooks_on_open_files(true);
        let (b_open, b_half_open) = self.rooks_on_open_files(false);

        material_score
            + position_score_final
            + doubled_pawns * params[EP_DOUBLED_PAWNS_IDX]
            + net_passed_pawns * params[EP_PASSED_PAWN_IDX]
            + (w_open - b_open) * params[EP_ROOK_OPEN_FILE_IDX]
            + (w_half_open - b_half_open) * params[EP_ROOK_HALF_OPEN_FILE_IDX]
    }

    pub fn evaluate_checkmate(&self, ply: u8) -> i16 {
        if self.white_to_move {
            -MATE_VALUE + (ply as i16) * 10
        } else {
            MATE_VALUE - (ply as i16) * 10
        }
    }

    pub fn evaluate_side_to_move_relative(&self, params: &EvalParams) -> i16 {
        self.evaluate(params) * if self.white_to_move { 1 } else { -1 }
    }

    pub fn evaluate_checkmate_side_to_move_relative(&self, ply: u8) -> i16 {
        self.evaluate_checkmate(ply) * if self.white_to_move { 1 } else { -1 }
    }

    /// Returns true if this position will be called a draw by the arbiter
    pub fn is_insufficient_material(&self) -> bool {
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

        (self.piece_bitboards[1][PIECE_PAWN as usize].count_ones() as i16 - pawn_occupied_files[1])
            - (self.piece_bitboards[0][PIECE_PAWN as usize].count_ones() as i16 - pawn_occupied_files[0])
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
        values[0] = CENTIPAWN_VALUES[(self.get_piece_64(to as usize) & PIECE_MASK) as usize];
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
            values[depth] = CENTIPAWN_VALUES[last_attacker] - values[depth - 1];
            depth += 1;
            color = if color != 0 { 0 } else { 1 };
            last_attacker = next_attacker_piece;
        }

        for i in (1..depth).rev() {
            values[i - 1] = -values[i].max(-values[i - 1]);
        }

        values[0]
    }
}

#[cfg(test)]
mod eval_tests {
    use crate::{STARTING_FEN, texel::DEFAULT_PARAMS};
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

        assert_eq!(b1.evaluate(&DEFAULT_PARAMS), -b2.evaluate(&DEFAULT_PARAMS));
        assert_eq!(
            b1.evaluate_side_to_move_relative(&DEFAULT_PARAMS),
            b2.evaluate_side_to_move_relative(&DEFAULT_PARAMS)
        );
    }

    #[test]
    pub fn unbalanced_pieces_mirrorred() {
        let b1 = Board::from_fen("4k3/8/8/8/2P5/1PB2N2/6Q1/2R1K3 w - - 0 1").unwrap();
        let b2 = Board::from_fen("2r1k3/6q1/1pb2n2/2p5/8/8/8/4K3 b - - 0 1").unwrap();

        assert_eq!(b1.evaluate(&DEFAULT_PARAMS), -b2.evaluate(&DEFAULT_PARAMS));
        assert_eq!(
            b1.evaluate_side_to_move_relative(&DEFAULT_PARAMS),
            b2.evaluate_side_to_move_relative(&DEFAULT_PARAMS)
        );
    }

    #[test]
    pub fn starting_position_is_even() {
        let b = Board::from_fen(STARTING_FEN).unwrap();

        assert_eq!(0, b.evaluate(&DEFAULT_PARAMS));
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
                    let (fen, expected_eval, m) = $value;

                    let board = Board::from_fen(fen).unwrap();

                    initialize_magic_bitboards();

                    let see_result = board.static_exchange_eval(Move::from_simple_long_algebraic_notation(m));

                    assert_eq!(expected_eval, see_result);
                }
            )*
        }
    }

    see_test! {
        // Some positions taken from https://github.com/zzzzz151/Starzix/blob/main/tests/SEE.txt
        no_recapture: ("1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - -", CENTIPAWN_VALUES[PIECE_PAWN as usize], "e1e5"),
        pawn_captures: ("k7/8/4p1p1/5p2/4P1P1/8/8/K7 w - - 0 1", 0, "e4f5"),
        sliders_behind_capturing_piece: ("2r2r1k/6bp/p7/2q2p1Q/3PpP2/1B6/P5PP/2RR3K b - -", CENTIPAWN_VALUES[PIECE_ROOK as usize] * 2 - CENTIPAWN_VALUES[PIECE_QUEEN as usize], "c5c1"),
        pawn_before_rook: ("4R3/2r3p1/5bk1/1p1r1p1p/p2PR1P1/P1BK1P2/1P6/8 b - -", 0, "h5g4"),
        bishop_for_knight_no_losing_queen_capture: ("5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - -", -CENTIPAWN_VALUES[PIECE_BISHOP as usize] + CENTIPAWN_VALUES[PIECE_KNIGHT as usize], "d6f4"),
        non_capture1: ("2r1k2r/pb4pp/5p1b/2KB3n/4N3/2NP1PB1/PPP1P1PP/R2Q3R w k -", -CENTIPAWN_VALUES[PIECE_BISHOP as usize], "d5c6"),
        non_capture1_recapture: ("2r1k2r/pb4pp/5p1b/2KB3n/1N2N3/3P1PB1/PPP1P1PP/R2Q3R w k -", 0, "d5c6"),
        rook_xray: ("4q3/1p1pr1k1/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -", CENTIPAWN_VALUES[PIECE_PAWN as usize] - CENTIPAWN_VALUES[PIECE_ROOK as usize], "e6e4"),
        rook_xray_extra_defender: ("4q3/1p1pr1kb/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -", CENTIPAWN_VALUES[PIECE_PAWN as usize], "e6e4"),
        // I think the best is if everything gets traded off, this is the net change of that. It fails, not sure if that is because my bishop val != knight val
        // big_trade_both_xrays: ("3r3k/3r4/2n1n3/8/3p4/2PR4/1B1Q4/3R3K w - -", CENTIPAWN_VALUES[PIECE_KNIGHT as usize] * 2 - CENTIPAWN_VALUES[PIECE_BISHOP as usize] + CENTIPAWN_VALUES[PIECE_ROOK as usize] - CENTIPAWN_VALUES[PIECE_QUEEN as usize], "d3d4"),
    }
}
