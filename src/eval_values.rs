use array_macro::array;

#[rustfmt::skip]
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 187, 162, 145, 137,  92,  46, -25,  35,
  48,  49,  53,  47,  55,  71,  59,  34,
   8,   4,   3,   8,  23,  22,   9,   4,
  -3,  -9,  -1,   4,   4,  13,   1, -11,
 -13, -15, -16, -15,  -9,  -5,   6, -10,
  -5,  -8, -14, -17, -13,  15,  26, -13,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
  78,  96,  94,  43,  76, 118, 176, 128,
  84,  80,  46,  23,   9,  15,  41,  42,
  45,  36,  19,  -7,  -9,  -5,  14,   6,
  25,  23,   1, -17, -12,  -1,   7,  -5,
  18,  10,   5,  -2,  -2,   2,  -7,  -5,
  32,  20,  16,   0,  16,   5,  -3,   6,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-114, -16, -24, -12,  17, -18, -22,-153,
 -12,  -1,  23,  38,  27,  36, -15,  12,
  -5,  13,  32,  52,  77,  76,  42,  12,
   1,   7,  29,  50,  29,  49,  27,  37,
  -7,   3,  22,  15,  28,  24,  37,   8,
 -30, -13,  -4,  10,  21,   6,   8, -13,
 -40, -22, -14,   3,   5,  -2,  -9,  -4,
 -81, -26, -34, -14, -11, -10, -15, -66,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  56,  44,  41,  43,  20,  35,  31,  24,
  13,  41,  26,  38,  26,  12,  42, -14,
  28,  30,  57,  43,  17,  21,  21,   9,
  24,  33,  57,  42,  58,  33,  30,  -2,
  24,  32,  60,  63,  48,  46,   9,  15,
  -2,  31,  49,  49,  34,  23,   1,   5,
  -2,  19,  14,  18,  12,  -2,  12, -40,
 -10, -12,  14,  13,  -3, -24, -21, -11,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -18, -25, -18, -21,  -5, -52,  24,   7,
 -20,  -6,  -5,  -1,   9,  -1, -25, -21,
  -8,  11,  21,  26,  34,  65,  40,  31,
 -10,  -3,  16,  43,  24,  29,   0,  -1,
  -2,   0,   6,  30,  21,  -3,   0,   9,
 -16,   8,   7,   7,   8,   6,   6,   2,
  -8,  -6,   9,  -8,   1,   5,  13,  -5,
 -36,  -8, -16, -20, -12, -18, -23, -26,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  51,  66,  51,  51,  29,  47,  23,  15,
  49,  44,  48,  36,  34,  37,  45,  20,
  35,  39,  28,  18,  21,  16,  23,   4,
  32,  45,  31,  33,  40,  20,  47,  26,
  26,  42,  47,  32,  31,  43,  33,  11,
  14,  29,  43,  37,  48,  25,  10,   0,
  10,   9,  12,  35,  14,  13,  -3, -24,
  27,  17,   8,  19,  12,   6,   9,  17,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  40,  42,  38,  37,  35,  39,  47,  40,
  22,  21,  37,  43,  46,  66,  53,  53,
   8,  20,  25,  32,  48,  66,  53,  30,
  -7,  -3,  13,  18,  20,  26,  23,   6,
 -20, -19, -12,  -4,  -3,  -3,   7, -19,
 -29, -21, -24, -19, -14, -11,   5, -24,
 -37, -31, -18, -15, -15,  -3, -13, -50,
 -16, -14,  -9,  -6,  -2,   0, -20, -24,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  79,  91,  98,  95, 102,  94,  88,  90,
 107, 109, 105,  99,  94,  78,  85,  81,
 110, 104, 102,  97,  85,  76,  81,  88,
 109, 109, 101,  96,  90,  86,  86,  92,
 103, 108, 107,  97,  92,  92,  80,  90,
  82,  82,  89,  76,  70,  63,  55,  74,
  71,  74,  71,  61,  55,  40,  56,  69,
  66,  60,  72,  65,  45,  45,  66,  67,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   5,  26,  27,  39,  55,  79,  76,  67,
  -9, -10,  21,  30,  45,  68,  52,  67,
   0,   6,  20,  29,  61, 100,  92,  67,
   0,   4,  15,  25,  41,  38,  48,  37,
  -4,   2,  15,  20,  21,  23,  32,  21,
  -5,   3,   5,  10,  12,  19,  25,  13,
  -8,   3,  10,  16,  17,   7,  -5, -16,
  -4,  -3,   1,  11,   2, -31, -51, -27,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 150, 148, 169, 173, 172, 125, 113, 117,
 149, 194, 205, 196, 188, 163, 155, 101,
 125, 174, 191, 205, 173, 117, 105, 107,
 127, 158, 179, 188, 173, 191, 169, 145,
 127, 155, 154, 168, 159, 163, 133, 158,
  97, 128, 153, 121, 136, 132, 118,  94,
  87,  86, 100,  71,  71,  59,  59,  71,
  74,  46,  62,  29,  66,  16,  66,  24,
];

#[rustfmt::skip]
/// This appears to have some anomalous values
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -28,  52,  32, -40,  65, -14,  51, -46,
  13,  -2,  54, -15, -38,  37,  -7,  17,
 -28,   8,   0,  -4, -27, -29, -36, -40,
  27,  44,  13,  10,  18,  27,  42,  27,
  16,  39,  36,  33,  29,  26,  11, -10,
   3,  19,  16,  15,   9,   7,   2, -14,
  -3,  -8,  12,  -7,  -6,  -3,  24,  22,
 -46,  11,   8, -40,  12, -28,  29,  16,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -76, -32,   1,  30,  -6,  39,  12,-112,
 -19,  36,  14,  43,  52,  39,  61,  -1,
  21,  47,  50,  56,  62,  78,  82,  51,
  -7,  21,  40,  40,  40,  41,  32,   8,
 -33,  -4,  12,  19,  22,  18,   8, -10,
 -48, -18,  -3,   9,  11,   6,  -6, -20,
 -39, -12, -15,  -7,  -7,  -8, -27, -57,
 -19, -49, -37, -21, -52, -25, -73, -93,
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
pub static CENTIPAWN_VALUES_MIDGAME: [i16; 7] = [0, 84, 331, 350, 503, 1030, 20000];
/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES_ENDGAME: [i16; 7] = [0, 110, 295, 322, 535, 997, 20000];

pub struct EvalFeature {
    pub midgame: i16,
    pub endgame: i16,
}

impl EvalFeature {
    pub const fn new(midgame: i16, endgame: i16) -> Self {
        Self { midgame, endgame }
    }
}

pub const DOUBLED_PAWN: EvalFeature = EvalFeature::new(-13, -20);
pub const PASSED_PAWNS: EvalFeature = EvalFeature::new(7, 18);
pub const ROOK_OPEN_FILES: EvalFeature = EvalFeature::new(29, -4);
pub const ROOF_HALF_OPEN_FILES: EvalFeature = EvalFeature::new(16, 24);
pub const BISHOP_PAIR: EvalFeature = EvalFeature::new(32, 88);
pub const PAWN_SHIELD: i16 = -9;
pub const CONNECTED_PAWNS: EvalFeature = EvalFeature::new(8, 4);
pub const PIECES_THREATENED_BY_PAWNS: EvalFeature = EvalFeature::new(55, 34);
pub const ISOLATED_PAWN: EvalFeature = EvalFeature::new(-10, -9);

#[rustfmt::skip]
pub static MOBILITY_ROOK_MIDGAME: [i16; 15] = [ -18, -13,  -9,  -5,  -4,   2,   5,   8,  11,  14,  18,  19,  24,  20,  26];
#[rustfmt::skip]
pub static MOBILITY_ROOK_ENDGAME: [i16; 15] = [   0,   0,  -1,  -1,   4,   5,  12,   7,  13,  14,  17,  17,  14,  14,  -3];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_MIDGAME: [i16; 14] = [ -29, -20, -12,  -6,   0,   7,  10,  12,  17,  19,  17,  15,  12,  13];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_ENDGAME: [i16; 14] = [   0,  -9, -15,  -6,   1,   8,  13,  19,  12,  15,  10,  10,  12,  12];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_MIDGAME: [i16; 9] = [ -41, -18,  -6,  -1,   6,  11,  16,  21,  12];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_ENDGAME: [i16; 9] = [   0,   1,   2,   5,  11,  17,  13,   1, -16];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_MIDGAME: [i16; 14] = [   2,  -1,   0,   3,   8,  13,  18,  24,  31,  37,  38,  26,  13,  14];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_ENDGAME: [i16; 14] = [   1,   2,   3,   4,   5,   6,   9,  14,  11,  10,  10,  12,  13,  14];
