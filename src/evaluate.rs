use array_macro::array;
use log::error;

use crate::{
    bitboard::{LIGHT_SQUARES, north_fill, south_fill},
    board::{
        Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_NONE,
        PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK, file_8x8,
    },
};

/// Indexed with piece code, so index 0 is no piece
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
 173, 147, 135, 128,  90,  41,   2,  46,
  39,  54,  43,  43,  50,  58,  82,  20,
   3,  11,  -2,  11,  19,  13,  12,  -7,
 -14,   5,  -5,   3,  -1,   8,   4, -16,
 -13,   0,  -8,  -8,   2,   2,  21,  -4,
 -20,  -6, -18, -16, -12,  17,  27, -15,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
  78, 116, 115,  70, 103, 136, 184, 132,
 108, 104,  81,  63,  57,  53,  64,  69,
  68,  52,  49,  10,  12,  21,  37,  29,
  46,  47,  24,   2,  12,  26,  37,  28,
  48,  35,  27,  27,  19,  28,  26,  19,
  62,  53,  40,   9,  42,  29,  28,  33,
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
  45,  57,  42,  42,  50,  71,  75,  14,
  31,  33,  57,  60,  50,  83,  61,  59,
  14,  52,  47,  38,  34,  57,  70,  38,
   4,   4,   6,  10,  22,  22,  33,   3,
 -19,  -2,  -4,   4,   3,   1,  13, -11,
 -15,  -4,  -7, -10, -10,  -6,  11,  -1,
 -27, -20,  -9, -11, -13,  -2,  -8, -42,
 -13,  -5,  -1,   1,   1,   0, -19, -40,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  16,  16,  32,  31,  21,  16,  25,  35,
  45,  28,  14,  14,  21,   4,  19,  11,
  44,  10,  22,  15,  30,  14,  19,  16,
  46,  47,  49,  41,  22,  41,  41,  43,
  49,  41,  46,  32,  27,  44,  27,  42,
  19,  16,  24,  23,  22,  26,   6,   8,
   9,  14,  26,  17,  22,   0,   2,  -5,
  14,  18,  23,  34,  11,  12,  37,  18,
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

pub static PIECE_SQUARE_TABLES: [[[i16; 64]; 12]; 2] = [
    // vertically flip each table for white
    array![x => array![y => ALL_PIECE_SQUARE_TABLES[x][y ^ 0b00111000]; 64]; 12],
    // Evaluate from white's perspective so negate each score for black
    array![x => array![y => -ALL_PIECE_SQUARE_TABLES[x][y]; 64]; 12],
];

static FILES: [u64; 8] = array![i => 0x0101010101010101 << i; 8];

// thread_local! {
//     pub static ISOLATED_PAWN_PENALTY: Cell<i16> = const { Cell::new(35) };
//     pub static DOUBLED_PAWN_PENALTY: Cell<i16> = const { Cell::new(25) };
// }

impl Board {
    pub fn evaluate(&self) -> i16 {
        let mut material_score = 0;
        let mut game_stage = self.game_stage;

        for i in 1..7 {
            material_score += CENTIPAWN_VALUES[i] * (self.piece_counts[0][i] as i16 - self.piece_counts[1][i] as i16);
        }

        let doubled_pawns = self.count_doubled_pawns();

        if game_stage > MIN_GAME_STAGE_FULLY_MIDGAME {
            game_stage = MIN_GAME_STAGE_FULLY_MIDGAME;
        }

        let position_score_final = ((self.piecesquare_midgame * game_stage)
            + (self.piecesquare_endgame * (MIN_GAME_STAGE_FULLY_MIDGAME - game_stage)))
            / (MIN_GAME_STAGE_FULLY_MIDGAME);

        let white_passed = self.white_passed_pawns();
        let white_passed_distance = (south_fill(white_passed) & !white_passed).count_ones() as i16;

        let black_passed = self.black_passed_pawns();
        let black_passed_distance = (north_fill(black_passed) & !black_passed).count_ones() as i16;

        let net_passed_pawns = white_passed_distance - black_passed_distance;

        let (w_open, w_half_open) = self.rooks_on_open_files(true);
        let (b_open, b_half_open) = self.rooks_on_open_files(false);

        material_score + position_score_final + doubled_pawns * 22 + net_passed_pawns * 10 + (w_open - b_open) * 18 + (w_half_open - b_half_open) * 17
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
        for color in 0..2 {
            for file in FILES {
                if self.piece_bitboards[color][PIECE_PAWN as usize] & file > 0 {
                    pawn_occupied_files[color] += 1;
                }
            }
        }

        (self.piece_counts[1][PIECE_PAWN as usize] as i16 - pawn_occupied_files[1])
            - (self.piece_counts[0][PIECE_PAWN as usize] as i16 - pawn_occupied_files[0])
    }
}

#[cfg(test)]
mod eval_tests {
    use crate::STARTING_FEN;

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
}
