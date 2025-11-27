use array_macro::array;

use crate::{
    bitboard::{BIT_SQUARES, LIGHT_SQUARES, north_fill, south_fill},
    board::{
        BISHOP_COLORS_DARK, BISHOP_COLORS_LIGHT, Board, COLOR_BLACK, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT,
        PIECE_MASK, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK,
    },
    magic_bitboard::{lookup_bishop_attack, lookup_rook_attack},
    moves::Move,
};

/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES: [i16; 7] = [0, 81, 292, 312, 443, 902, 20000];

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
 167, 148, 137, 130,  86,  59,  16,  55,
  39,  53,  51,  48,  52,  65,  74,  31,
   0,  12,   8,  15,  26,  25,  17,   0,
 -12,   3,  -4,   2,   3,  12,   9, -12,
 -14,  -3,  -9,  -8,   0,   1,  20,  -4,
 -20,  -9, -17, -27, -14,  11,  29, -20,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 180, 194, 199, 152, 176, 198, 245, 218,
 133, 126, 101,  83,  72,  64,  89,  88,
  84,  67,  53,  21,  18,  26,  51,  42,
  60,  52,  28,  10,  15,  28,  42,  29,
  56,  45,  29,  27,  26,  28,  32,  25,
  69,  62,  41,  31,  42,  37,  36,  43,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-124, -21,  -7, -15,  24, -46, -39,-118,
 -10,   4,  22,  38,  36,  38, -35, -10,
   6,  27,  43,  59,  78,  81,  36,   6,
   1,  13,  35,  57,  34,  64,  21,  30,
 -11,   7,  24,  14,  27,  22,  30,  -7,
 -25, -11,   3,  12,  28,   6,  13, -14,
 -49, -23, -14,  -5,  -2,   0, -15, -33,
 -81, -42, -35, -25, -25, -17, -38, -66,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  -3, -31, -17, -25, -80, -17, -30, -65,
 -62, -34, -44, -42, -44, -70, -54, -88,
 -43, -48, -23, -39, -62, -60, -43, -69,
 -53, -31, -24, -37, -20, -43, -41, -87,
 -53, -33, -12,  -8, -23, -38, -65, -63,
 -87, -32, -22, -22, -35, -39, -74, -89,
 -63, -48, -46, -45, -51, -54, -80,-105,
 -87, -93, -53, -64, -76, -84,-101,-110,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -13, -29, -36, -18, -37, -57, -14,  20,
  -7,   0,   6, -10,   9,   4,  -8, -19,
  -1,  17,  27,  35,  37,  66,  34,  26,
   3,   6,  18,  43,  33,  29,   1,  -2,
 -13,  11,   7,  29,  20,   5,  -1,  -1,
 -12,   5,  11,   9,   8,   5,   5,  -2,
 -13,  -5,   2,  -9,   0,  -4,  14, -16,
 -37, -26, -28, -22, -19, -31, -23, -25,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  14,   7,  10,   2,  -3,  -5, -19, -63,
 -16,  -4,  -6, -14, -17, -20, -19, -45,
 -20, -12, -19, -42, -33, -32, -32, -49,
 -30, -10, -22, -17, -15, -26,  -9, -22,
 -20, -17,  -2,  -6, -14, -12, -15, -43,
 -47, -21,  -3, -11,   3, -27, -53, -53,
 -50, -39, -42, -24, -31, -38, -68,-131,
 -40, -23, -54, -31, -43, -44, -47, -32,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  50,  47,  43,  37,  35,  34,  47,  39,
  25,  28,  47,  56,  46,  68,  54,  53,
  11,  23,  29,  36,  40,  65,  50,  22,
  -4,  -4,  10,  13,  15,  21,  14,  -7,
 -23, -15,  -9,  -3,  -5,  -5,   0, -22,
 -30, -21, -19, -21, -18, -19,  -1, -22,
 -32, -27, -15, -18, -16,  -6, -20, -49,
 -17, -17,  -9,  -5,  -3,  -4, -40, -23,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  51,  75,  76,  78,  90, 104,  88,  75,
  88,  87,  78,  67,  71,  65,  70,  59,
  86,  77,  75,  64,  62,  60,  62,  66,
  86,  90,  76,  72,  67,  75,  74,  80,
  84,  79,  75,  70,  66,  77,  60,  67,
  53,  52,  56,  60,  51,  57,  34,  43,
  44,  44,  50,  42,  40,  29,  29,  33,
  64,  59,  70,  70,  47,  52,  94,  48,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   4,  21,  28,  39,  68,  78,  71,  59,
  -6, -11,  23,  30,  36,  72,  46,  64,
   2,   6,  21,  35,  54, 102,  86,  51,
  -7,  -1,   9,  24,  31,  40,  43,  27,
  -3,   2,   6,  19,  16,  14,  22,  10,
  -6,   3,   3,   7,   7,  12,  17,  -3,
 -17,  -5,   9,   6,  11,   1, -16, -37,
 -11, -12,  -8,   5,  -7, -38, -49, -27,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  84, 114, 139, 134, 114,  90,  70,  68,
 120, 154, 148, 167, 178, 107, 122,  84,
  93, 115, 131, 158, 148,  65,  39,  64,
 102, 111, 162, 147, 146, 156, 128,  96,
  98, 128, 124, 141, 138, 148, 116, 128,
  54,  64, 118,  89,  99,  95,  57,  80,
  61,  44,  54,  44,  45,   4,  -6,  29,
  56,  53,  54,   5,  57, -34,   4, -22,
];

#[rustfmt::skip]
/// This appears to have some anomalous values
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -28,  52,  32, -40,  65, -14,  51, -46,
  13,  -4,  58, -15, -39,  35,  -7,  18,
 -28,   8,   0,  -4, -27, -30, -41, -40,
  27,  45,  13,   9,  18,  24,  42,  29,
  16,  40,  36,  31,  29,  25,   3,  -3,
   6,  22,  17,  14,  18,   7,   6, -15,
  20,   9,   8, -13,  -8,  -3,  23,  22,
 -36,  21,   1, -46,  -1, -38,  32,  20,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -69, -38,  -2,  23, -11,  22,   2, -64,
  -9,  11,  27,  47,  52,   7,  46,  21,
  13,  34,  48,  47,  58,  71,  65,  44,
  -6,  18,  33,  37,  37,  35,  23,   8,
 -35, -10,   4,  16,  21,  19,  10,  -6,
 -45, -24,  -4,   7,  10,   9,  -3, -13,
 -51, -19, -13,   2,   1,   2, -15, -40,
  -1, -50, -24, -14, -37,  -9, -60, -78,
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

static BISHOP_GUARDED_PROMOTION_FILES: [[u64; 4]; 2] = [
    [0, 0xAAAAAAAAAAAAAAAA, 0x5555555555555555, 0xFFFFFFFFFFFFFFFF],
    [0, 0x5555555555555555, 0xAAAAAAAAAAAAAAAA, 0xFFFFFFFFFFFFFFFF],
];

impl Board {
    pub fn evaluate(&self) -> i16 {
        let mut material_score = 0;
        for i in 1..7 {
            material_score += CENTIPAWN_VALUES[i] * (self.piece_counts[0][i] as i16 - self.piece_counts[1][i] as i16);
        }

        let mut capped_game_stage = self.game_stage;
        if capped_game_stage > MIN_GAME_STAGE_FULLY_MIDGAME {
            capped_game_stage = MIN_GAME_STAGE_FULLY_MIDGAME;
        }

        let position_score_final = ((self.piecesquare_midgame * capped_game_stage)
            + (self.piecesquare_endgame * (MIN_GAME_STAGE_FULLY_MIDGAME - capped_game_stage)))
            / (MIN_GAME_STAGE_FULLY_MIDGAME);

        let doubled_pawns = self.count_doubled_pawns();

        let white_passed = self.white_passed_pawns();
        let white_passed_distance = (south_fill(white_passed) & !white_passed).count_ones() as i16;

        let black_passed = self.black_passed_pawns();
        let black_passed_distance = (north_fill(black_passed) & !black_passed).count_ones() as i16;

        let net_passed_pawns = white_passed_distance - black_passed_distance;

        let (w_open, w_half_open) = self.rooks_on_open_files(true);
        let (b_open, b_half_open) = self.rooks_on_open_files(false);

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

        let mut pawn_shield_eval = 0;
        let game_stage_for_pawn_shield = if self.game_stage <= ENDGAME_GAME_STAGE_FOR_QUIESCENSE {
            0
        } else {
            self.game_stage - ENDGAME_GAME_STAGE_FOR_QUIESCENSE
        };
        if game_stage_for_pawn_shield > 0 {
            // How much pawn shield each side is missing. Positive: white is missing more
            let net_pawn_shield_penalty = (6 - self.score_pawn_shield(0)) - (6 - self.score_pawn_shield(1));
            pawn_shield_eval =
                (game_stage_for_pawn_shield * net_pawn_shield_penalty * -8) / (MAX_GAME_STAGE - ENDGAME_GAME_STAGE_FOR_QUIESCENSE);
        }

        material_score
            + position_score_final
            + doubled_pawns * 22
            + net_passed_pawns * 9
            + (w_open - b_open) * 26
            + (w_half_open - b_half_open) * 20
            + bishop_pair * 23
            + pawn_shield_eval
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
                    let (fen, expected_eval, m) = $value;

                    let board = Board::from_fen(fen).unwrap();

                    initialize_magic_bitboards();

                    let see_result = board.static_exchange_eval(Move::from_simple_long_algebraic_notation(m, 0));

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
