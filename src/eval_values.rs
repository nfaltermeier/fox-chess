use array_macro::array;

#[rustfmt::skip]
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 167, 148, 137, 130,  86,  59,  16,  55,
  41,  52,  51,  47,  52,  67,  74,  34,
   7,   2,   3,   4,  21,  21,  15,   5,
  -6,  -8,  -4,   2,   1,  11,   1,  -8,
 -11, -17, -15, -15,  -5,  -1,   9,  -7,
  -8, -11, -14, -25, -15,  16,  28, -12,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 150, 162, 163, 109, 148, 167, 222, 174,
  91,  80,  53,  31,   9,  19,  34,  42,
  41,  26,  13, -18, -20, -15,   0,  -3,
  17,  16, -10, -26, -20, -13,  -8, -18,
  10,   1,  -6, -16, -15, -14, -19, -19,
  25,  13,   6,  23,  11,  -7, -15,  -5,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-124, -21,  -7, -15,  24, -46, -39,-118,
 -10,   2,  20,  41,  34,  38, -32, -10,
   4,  19,  42,  59,  80,  81,  41,   9,
   1,   9,  37,  55,  28,  58,  28,  35,
 -10,   7,  22,  14,  29,  25,  36,   6,
 -30, -12,  -3,  12,  26,   5,   9, -19,
 -46, -23, -16,  -1,  -1,   0, -15,  -3,
 -81, -34, -37, -21, -20, -12, -36, -66,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  27,   5,   4,  10,  -3,  26,  -3, -40,
 -11,  12,   1,   6,  -2, -26,   9, -25,
  -8,   0,  21,  13, -13, -21, -18, -30,
  -1,   1,  27,  11,  31,  -5,   5, -26,
  -4,  12,  32,  35,  25,  13, -20, -11,
 -22,  16,  24,  26,   6,   1, -20, -14,
 -34, -12,   3,   7,  -2, -16, -19, -58,
 -25, -46, -20,  -5, -21, -39, -44, -69,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -13, -29, -36, -18, -37, -57, -14,  20,
  -8,   0,   1,  -9,   9,   4, -13, -19,
  -2,  14,  21,  27,  34,  66,  39,  29,
  -5,  -1,  16,  44,  23,  27,  -3,  -2,
 -12,   1,   7,  26,  16,  -5,   1,   3,
 -20,   6,   5,   4,   7,   4,   5,   0,
 -12,  -5,   4, -11,   0,   4,  15, -13,
 -36, -22, -21, -29, -18, -22, -23, -27,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  28,  44,  39,  30,  27,  33,  16, -10,
  11,  21,  23,  18,  11,   7,  16, -13,
  13,  14,   7,   2,  -9,  -8,   4, -13,
  17,  22,  11,   8,  15,  -1,  26,  -1,
  11,  22,  24,  14,  24,  29,  11,   2,
  -5,   8,  23,  20,  32,   7,  -8, -17,
 -14,  -2,   0,  19,  -1, -12, -29, -44,
   0,  16,  -8,   5,  -1, -15,   0,  13,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  46,  46,  40,  38,  35,  37,  47,  40,
  20,  20,  40,  51,  45,  67,  53,  53,
   9,  20,  25,  33,  45,  66,  51,  23,
  -9,  -3,  11,  15,  14,  26,  17,  -2,
 -23, -17, -11,  -3,  -5,  -4,   2, -21,
 -29, -25, -22, -23, -17, -13,   5, -22,
 -34, -27, -17, -18, -18,  -1, -14, -49,
 -14, -13,  -9,  -6,  -2,   1, -20, -21,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  44,  54,  62,  61,  65,  57,  56,  60,
  70,  72,  63,  54,  57,  34,  42,  42,
  70,  63,  63,  57,  45,  38,  39,  54,
  71,  70,  64,  58,  50,  47,  45,  60,
  66,  69,  67,  60,  53,  55,  50,  53,
  44,  45,  47,  43,  40,  36,  21,  35,
  33,  26,  31,  31,  25,   9,  20,  32,
  42,  36,  49,  46,  27,  24,  52,  45,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   4,  21,  28,  39,  67,  78,  71,  59,
  -7, -11,  24,  30,  36,  72,  46,  64,
  -2,   6,  20,  35,  54, 105,  87,  59,
  -8,   1,  13,  23,  34,  40,  43,  35,
  -7,   2,   9,  16,  20,  17,  23,  17,
  -7,  -1,   3,   4,   4,  10,  19,  -1,
 -17,  -2,   4,   5,   8,   1, -16, -37,
 -11, -15,  -9,  -1,  -8, -38, -49, -27,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  95, 114, 127, 142,  99,  81,  73,  83,
  92, 134, 142, 147, 151,  88, 103,  49,
  67, 104, 117, 132, 116,  58,  45,  25,
  65, 113, 134, 140, 134, 130, 102,  82,
  71, 105, 104, 121, 114, 126,  98,  86,
  58,  63, 103,  88,  88,  92,  86,  59,
  53,  34,  61,  50,  33,  -2,  15,  50,
  49,  46,  50,  28,  49,   8,  10,  -5,
];

#[rustfmt::skip]
/// This appears to have some anomalous values
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -28,  52,  32, -40,  65, -14,  51, -46,
  13,  -2,  54, -15, -38,  37,  -7,  17,
 -28,   8,   0,  -4, -27, -29, -38, -40,
  27,  44,  13,  10,  18,  26,  42,  27,
  16,  40,  36,  32,  30,  25,   6,  -7,
   3,  21,  16,  15,  15,   8,   7, -15,
  13,   1,   9, -14,  -6,  -4,  26,  24,
 -40,  12,   0, -44,  13, -34,  33,  21,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -69, -36,  -2,  23, -11,  25,  -2, -64,
  -9,  31,   1,  43,  50,  27,  49,   4,
  19,  38,  48,  51,  59,  73,  78,  48,
  -6,  15,  34,  39,  40,  39,  27,   5,
 -35, -10,  10,  19,  24,  21,  11, -12,
 -47, -22,  -3,   9,  12,  10,  -4, -16,
 -51, -18, -10,   0,   0,   0, -19, -46,
  -3, -54, -24, -14, -47, -16, -64, -87,
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

/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES_MIDGAME: [i16; 7] = [0, 79, 293, 313, 449, 921, 20000];
/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES_ENDGAME: [i16; 7] = [0, 120, 293, 313, 516, 994, 20000];

pub struct EvalFeature {
    pub midgame: i16,
    pub endgame: i16,
}

impl EvalFeature {
    pub const fn new(midgame: i16, endgame: i16) -> Self {
        Self { midgame, endgame }
    }
}

pub const DOUBLED_PAWN: EvalFeature = EvalFeature::new(-10, -24);
pub const PASSED_PAWNS: EvalFeature = EvalFeature::new(7, 16);
pub const ROOK_OPEN_FILES: EvalFeature = EvalFeature::new(32, 1);
pub const ROOF_HALF_OPEN_FILES: EvalFeature = EvalFeature::new(16, 34);
pub const BISHOP_PAIR: EvalFeature = EvalFeature::new(29, 88);
pub const PAWN_SHIELD: i16 = -9;
pub const CONNECTED_PAWNS: EvalFeature = EvalFeature::new(8, 4);
pub const PIECES_THREATENED_BY_PAWNS: EvalFeature = EvalFeature::new(56, 34);
pub const ISOLATED_PAWN: EvalFeature = EvalFeature::new(-11, -6);

pub static MOBILITY_ROOK_MIDGAME: [i16; 15] = [-10, -7, -4, -1, -2, 3, 5, 6, 9, 12, 13, 15, 16, 16, 19];
pub static MOBILITY_ROOK_ENDGAME: [i16; 15] = [0, 1, 0, -1, 4, 5, 6, 4, 8, 11, 15, 15, 14, 13, -2];
pub static MOBILITY_BISHOP_MIDGAME: [i16; 14] = [-32, -19, -12, -7, 0, 7, 10, 12, 16, 19, 16, 14, 12, 13];
pub static MOBILITY_BISHOP_ENDGAME: [i16; 14] = [0, -6, -12, -6, 1, 7, 10, 15, 12, 13, 10, 10, 12, 12];
pub static MOBILITY_KNIGHT_MIDGAME: [i16; 9] = [-38, -17, -6, 0, 5, 9, 14, 15, 5];
pub static MOBILITY_KNIGHT_ENDGAME: [i16; 9] = [0, 1, 2, 5, 7, 12, 7, 5, -16];
