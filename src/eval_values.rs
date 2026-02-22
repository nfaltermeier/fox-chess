use array_macro::array;

#[rustfmt::skip]
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 188, 160, 145, 137,  88,  41, -28,  31,
  46,  49,  51,  47,  54,  68,  60,  32,
   7,   4,   2,   7,  23,  22,   8,   3,
  -4,  -8,  -2,   5,   4,  14,   1, -12,
 -12, -14, -16, -14,  -7,  -2,   7,  -9,
  -7, -10, -17, -18, -13,  15,  25, -14,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
  76,  93,  92,  42,  73, 118, 173, 127,
  85,  80,  49,  22,   7,  18,  40,  42,
  48,  38,  22,  -4,  -7,  -2,  14,   8,
  27,  26,   3, -15, -11,   0,   9,  -1,
  21,  14,  10,  -1,   1,   5,  -3,  -2,
  34,  22,  18,  -1,  16,   7,  -2,   8,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-114, -24, -24, -10,  19, -20, -23,-146,
 -11,  -1,  24,  36,  28,  35, -16,   9,
  -6,  12,  32,  53,  77,  76,  42,  12,
   1,   6,  31,  48,  26,  47,  25,  36,
  -5,   3,  21,  14,  27,  24,  37,   6,
 -29, -10,  -4,  10,  23,   6,   7, -12,
 -40, -22, -13,   4,   7,  -1,  -9,  -4,
 -81, -23, -32, -14, -12, -10, -12, -66,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  54,  41,  38,  42,  17,  36,  28,  31,
  14,  40,  22,  40,  21,  12,  43, -16,
  29,  30,  57,  40,  17,  18,  14,   5,
  21,  30,  55,  42,  58,  33,  28,   4,
  24,  31,  59,  63,  46,  44,  10,  13,
  -2,  28,  47,  47,  29,  21,   1,   1,
  -4,  19,  12,  16,   9,  -6,  10, -38,
  -7, -15,  14,  15,  -3, -22, -28,  -8,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -11, -25, -15, -15,  -3, -51,  21,   8,
 -19,  -5,  -2,  -2,   8,  -2, -27, -25,
  -7,  11,  21,  25,  33,  64,  40,  27,
 -10,  -1,  16,  42,  23,  26,  -4,  -4,
  -1,  -1,   6,  28,  20,  -6,  -1,   4,
 -14,   8,   8,   6,   7,   6,   7,   0,
  -7,  -6,   8,  -8,   1,   6,  14,  -5,
 -36,  -7, -17, -20, -11, -18, -23, -26,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  44,  65,  49,  45,  28,  45,  22,  16,
  48,  43,  43,  37,  33,  36,  44,  20,
  33,  38,  27,  19,  19,  15,  24,   9,
  30,  42,  30,  32,  39,  22,  51,  27,
  28,  42,  47,  32,  31,  44,  34,  17,
  12,  28,  40,  37,  48,  24,   8,   3,
  10,  13,  11,  34,  13,  10,  -6, -21,
  23,  14,   9,  21,  10,   5,  10,  16,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  39,  42,  38,  37,  35,  39,  47,  40,
  21,  21,  37,  42,  46,  66,  53,  53,
   7,  21,  24,  31,  48,  66,  53,  30,
  -7,  -3,  12,  17,  19,  25,  24,   6,
 -20, -20, -11,  -1,  -2,  -4,   7, -19,
 -30, -20, -24, -19, -11, -12,   3, -24,
 -38, -30, -15, -12, -13,  -3, -13, -50,
 -15, -14,  -7,  -5,  -1,   1, -21, -23,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  79,  89,  97,  94, 101,  92,  86,  88,
 107, 108, 104,  99,  93,  77,  84,  79,
 109, 102, 102,  97,  83,  73,  79,  87,
 106, 107, 102,  97,  90,  85,  85,  91,
 101, 107, 105,  93,  88,  90,  79,  88,
  81,  79,  86,  75,  67,  63,  53,  70,
  70,  71,  68,  56,  52,  42,  55,  69,
  64,  58,  69,  63,  42,  42,  64,  66,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   5,  27,  27,  39,  56,  79,  77,  67,
  -7,  -8,  21,  29,  46,  67,  52,  67,
   1,   8,  20,  30,  62,  98,  92,  67,
   2,   6,  15,  24,  39,  38,  47,  34,
  -1,   4,  13,  18,  19,  21,  29,  20,
  -2,   5,   8,  11,  12,  19,  23,  13,
  -7,   2,  10,  17,  18,  12,  -5, -14,
  -2,  -3,   2,  12,   5, -29, -51, -27,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 145, 141, 167, 171, 166, 112, 112, 116,
 142, 186, 202, 193, 183, 161, 149, 103,
 113, 168, 183, 201, 168, 115, 102,  99,
 125, 150, 176, 188, 170, 185, 165, 147,
 112, 150, 150, 164, 156, 163, 136, 153,
  86, 116, 139, 113, 132, 123, 113,  85,
  88,  86,  92,  66,  64,  33,  61,  61,
  62,  40,  61,  21,  53,   9,  65,  20,
];

#[rustfmt::skip]
/// This appears to have some anomalous values
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -28,  52,  32, -40,  65, -14,  51, -46,
  13,  -2,  54, -15, -38,  37,  -7,  17,
 -28,   8,   0,  -4, -27, -28, -36, -40,
  27,  43,  13,  10,  17,  27,  42,  27,
  16,  39,  36,  34,  29,  24,  11, -11,
   3,  17,  16,  16,  12,  10,   2, -15,
  -2,  -8,  11,  -6,  -6,  -1,  24,  18,
 -46,  10,   6, -40,   9, -29,  26,  12,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -78, -31,   1,  30,  -6,  39,  14,-114,
 -17,  34,  16,  45,  52,  38,  60,  -1,
  20,  49,  50,  55,  64,  79,  82,  50,
  -8,  20,  41,  41,  40,  42,  31,   6,
 -33,  -6,  10,  18,  22,  19,   8, -11,
 -47, -18,  -2,   8,  10,   5,  -7, -19,
 -38, -10, -14,  -8,  -7,  -9, -28, -54,
 -21, -50, -39, -20, -51, -25, -71, -89,
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
pub static CENTIPAWN_VALUES_MIDGAME: [i16; 7] = [0, 86, 334, 353, 508, 1042, 20000];
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

pub const DOUBLED_PAWN: EvalFeature = EvalFeature::new(-14, -23);
pub const PASSED_PAWNS: EvalFeature = EvalFeature::new(7, 19);
pub const ROOK_OPEN_FILES: EvalFeature = EvalFeature::new(30, -6);
pub const ROOF_HALF_OPEN_FILES: EvalFeature = EvalFeature::new(15, 24);
pub const BISHOP_PAIR: EvalFeature = EvalFeature::new(32, 88);
pub const PAWN_SHIELD: i16 = -9;
pub const CONNECTED_PAWNS: EvalFeature = EvalFeature::new(6, -2);
pub const PIECES_THREATENED_BY_PAWNS: EvalFeature = EvalFeature::new(55, 34);
pub const ISOLATED_PAWN: EvalFeature = EvalFeature::new(-11, -14);

#[rustfmt::skip]
pub static MOBILITY_ROOK_MIDGAME: [i16; 15] = [ -17, -12,  -7,  -4,  -3,   3,   5,   8,  11,  14,  17,  18,  24,  20,  27];
#[rustfmt::skip]
pub static MOBILITY_ROOK_ENDGAME: [i16; 15] = [   0,  -1,  -1,  -2,   4,   5,  12,   7,  13,  15,  17,  19,  14,  14,  -3];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_MIDGAME: [i16; 14] = [ -28, -19, -11,  -6,  -1,   5,   9,  11,  16,  18,  18,  16,  12,  13];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_ENDGAME: [i16; 14] = [   0, -12, -17,  -6,   2,   8,  14,  19,  13,  15,  10,  10,  12,  12];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_MIDGAME: [i16; 9] = [ -41, -19,  -7,  -1,   6,  12,  17,  21,  12];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_ENDGAME: [i16; 9] = [   0,   1,   2,   5,  11,  17,  13,   1, -16];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_MIDGAME: [i16; 14] = [   7,   2,   2,   4,   8,  13,  17,  22,  30,  37,  39,  26,  13,  14];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_ENDGAME: [i16; 14] = [   1,   2,   3,   4,   5,   6,   9,  14,  11,  10,  10,  12,  13,  14];

/// Knight, bishop, rook, queen
pub static KING_ATTACK_UNIT_PIECE_VALUES: [u16; 4] = [6, 36, 49, 57];
