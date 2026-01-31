use array_macro::array;

#[rustfmt::skip]
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 167, 148, 137, 130,  86,  59,  16,  55,
  41,  52,  51,  47,  52,  67,  74,  34,
   7,   2,   3,   4,  20,  21,  15,   5,
  -5,  -8,  -4,   2,   1,  11,   1,  -8,
 -11, -16, -15, -15,  -5,  -1,   9,  -7,
  -7, -10, -14, -22, -14,  17,  28, -12,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 150, 161, 162, 109, 147, 166, 221, 173,
  91,  80,  53,  29,   9,  18,  34,  42,
  41,  29,  12, -16, -16, -14,   0,  -3,
  17,  16,  -9, -26, -19, -12,  -7, -17,
  11,   1,  -5, -15, -13, -14, -19, -18,
  26,  15,   8,  22,  10,  -7, -14,  -5,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-124, -21,  -7, -15,  24, -46, -39,-118,
 -10,   1,  20,  41,  32,  38, -32, -10,
   3,  17,  40,  56,  80,  80,  41,  10,
   1,   7,  36,  54,  25,  56,  26,  36,
  -9,   6,  21,  14,  27,  24,  37,   7,
 -30, -12,  -4,  10,  25,   6,   8, -18,
 -45, -23, -16,   2,   1,   0, -13,  -1,
 -81, -32, -37, -18, -17, -11, -35, -66,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  27,  12,   5,  10,  -6,  26,  -3, -40,
 -13,  13,   4,   6,  -1, -29,   9, -24,
  -9,   2,  23,  17, -13, -18, -19, -30,
   0,   9,  28,  12,  37,   0,   7, -24,
  -4,  11,  33,  35,  28,  17, -23,  -8,
 -24,  15,  25,  27,  10,   3, -18, -14,
 -30, -15,   2,   4,  -7, -17, -14, -57,
 -21, -45, -20,  -5, -22, -41, -39, -67,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -13, -29, -36, -18, -37, -57, -14,  20,
  -8,   0,   0,  -9,   9,   4, -13, -19,
  -2,  14,  21,  26,  34,  66,  39,  29,
  -5,  -1,  16,  43,  22,  26,  -3,  -2,
 -12,   0,   6,  25,  16,  -6,   1,   3,
 -20,   6,   5,   4,   8,   6,   5,   0,
 -12,  -5,   5,  -9,   0,   4,  15, -13,
 -36, -21, -19, -29, -17, -23, -23, -27,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  32,  48,  40,  31,  27,  33,  16, -10,
  16,  21,  23,  19,   9,   7,  17, -13,
  10,  17,   9,   4,  -7,  -1,   7, -13,
  19,  23,   9,  12,  20,   0,  32,   1,
   7,  22,  28,  18,  22,  31,  11,   2,
  -2,   9,  24,  21,  31,   3,  -4, -12,
  -7,  -5,  -5,  17,  -2,  -9, -22, -44,
   4,  19,  -2,   6,   2, -13,   3,  15,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  46,  46,  40,  38,  35,  37,  47,  40,
  20,  20,  40,  51,  45,  67,  53,  53,
   8,  20,  25,  32,  45,  66,  51,  23,
  -9,  -3,  11,  15,  14,  26,  17,  -2,
 -23, -17, -11,  -3,  -5,  -4,   2, -21,
 -29, -25, -22, -23, -17, -12,   5, -22,
 -34, -27, -17, -17, -18,   0, -13, -49,
 -15, -13,  -9,  -6,  -1,   1, -20, -21,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  45,  54,  62,  63,  65,  56,  57,  58,
  70,  72,  63,  55,  56,  32,  42,  41,
  71,  64,  64,  56,  45,  38,  40,  53,
  70,  69,  65,  59,  52,  47,  45,  61,
  64,  67,  67,  59,  51,  54,  51,  54,
  45,  45,  48,  44,  41,  35,  21,  34,
  34,  27,  33,  31,  24,  11,  20,  32,
  42,  34,  50,  45,  22,  24,  49,  45,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   1,  21,  28,  36,  64,  78,  71,  59,
  -9, -17,  20,  28,  36,  69,  44,  65,
  -4,   4,  14,  31,  54, 107,  87,  67,
  -9,   1,  13,  17,  32,  38,  48,  37,
  -7,  -1,   8,  13,  18,  18,  26,  21,
  -9,  -1,   3,   5,   7,  15,  22,   3,
 -16,  -1,   5,  12,  14,   6, -13, -34,
  -9, -11,  -2,   6,   0, -37, -49, -27,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  91,  96, 118, 129,  94,  77,  65,  75,
  98, 142, 128, 131, 130,  76,  96,  40,
  70,  99, 123, 119, 111,  42,  49,  26,
  78, 102, 115, 131, 128, 132,  97,  95,
  67, 105,  98, 118, 110, 125,  94,  89,
  53,  59, 102,  81,  86,  83,  81,  61,
  52,  35,  61,  27,  22,  -6,  12,  53,
  37,  39,  29,   4,  38,  13,  14,  -4,
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
  13,   1,   9, -14,  -6,  -4,  26,  25,
 -40,  12,   1, -44,  13, -33,  31,  21,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -70, -35,  -2,  22, -11,  25,  -3, -64,
  -9,  31,   0,  39,  48,  27,  51,   3,
  20,  38,  47,  48,  59,  75,  76,  45,
 -11,  16,  34,  38,  41,  41,  26,   5,
 -36,  -8,  14,  20,  24,  21,  12, -12,
 -48, -19,   0,  12,  14,  10,  -5, -17,
 -52, -17, -10,   1,   0,   1, -20, -49,
  -4, -54, -24, -11, -45, -15, -63, -89,
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
pub static CENTIPAWN_VALUES_MIDGAME: [i16; 7] = [0, 78, 293, 313, 449, 921, 20000];
/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES_ENDGAME: [i16; 7] = [0, 119, 293, 313, 516, 994, 20000];

pub struct EvalFeature {
    pub midgame: i16,
    pub endgame: i16,
}

impl EvalFeature {
    pub const fn new(midgame: i16, endgame: i16) -> Self {
        Self { midgame, endgame }
    }
}

pub const DOUBLED_PAWN: EvalFeature = EvalFeature::new(-10, -23);
pub const PASSED_PAWNS: EvalFeature = EvalFeature::new(7, 16);
pub const ROOK_OPEN_FILES: EvalFeature = EvalFeature::new(32, 1);
pub const ROOF_HALF_OPEN_FILES: EvalFeature = EvalFeature::new(16, 34);
pub const BISHOP_PAIR: EvalFeature = EvalFeature::new(29, 88);
pub const PAWN_SHIELD: i16 = -10;
pub const CONNECTED_PAWNS: EvalFeature = EvalFeature::new(8, 4);
pub const PIECES_THREATENED_BY_PAWNS: EvalFeature = EvalFeature::new(56, 34);
pub const ISOLATED_PAWN: EvalFeature = EvalFeature::new(-10, -6);

#[rustfmt::skip]
pub static MOBILITY_ROOK_MIDGAME: [i16; 15] = [ -12,  -8,  -5,  -2,  -2,   3,   5,   6,   9,  12,  13,  15,  16,  16,  19];
#[rustfmt::skip]
pub static MOBILITY_ROOK_ENDGAME: [i16; 15] = [   0,   1,   0,  -1,   4,   5,   6,   4,   8,  11,  15,  15,  14,  13,  -2];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_MIDGAME: [i16; 14] = [ -34, -21, -13,  -7,   0,   6,  10,  13,  15,  19,  17,  15,  12,  13];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_ENDGAME: [i16; 14] = [   0,  -7, -13,  -6,   1,   8,  11,  16,  13,  14,  10,  10,  12,  12];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_MIDGAME: [i16; 9] = [ -40, -18,  -7,  -1,   5,  10,  13,  16,   7];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_ENDGAME: [i16; 9] = [   0,   0,   2,   6,   8,  14,   9,   4, -17];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_MIDGAME: [i16; 14] = [   0,  -7,  -5,  -4,   1,   5,   9,  15,  22,  28,  34,  26,  13,  14];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_ENDGAME: [i16; 14] = [   1,   2,   3,   4,   5,   6,   7,  10,   9,  10,  10,  12,  13,  14];
