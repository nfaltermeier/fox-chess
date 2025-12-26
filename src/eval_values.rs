use array_macro::array;

#[rustfmt::skip]
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 167, 148, 137, 130,  86,  59,  16,  55,
  39,  53,  51,  44,  52,  67,  74,  31,
   5,  12,   8,  11,  28,  25,  20,   2,
  -9,   2,  -2,   3,   3,  13,  14, -11,
 -15,  -9, -14, -15,  -6,   1,  17,  -9,
 -12,  -4, -13, -31, -17,  16,  35, -17,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 156, 172, 172, 118, 159, 177, 232, 182,
  93,  85,  58,  37,  16,  24,  42,  47,
  42,  26,  11, -20, -22, -14,   5,  -2,
  17,  15, -12, -27, -22, -13,  -7, -18,
  11,   4,  -7, -15, -14, -15, -16, -19,
  27,  17,   7,  25,  11,  -6, -10,  -4,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-124, -21,  -7, -15,  24, -46, -39,-118,
 -10,   4,  22,  40,  36,  38, -33, -10,
   6,  27,  43,  64,  80,  81,  40,   6,
   1,  14,  41,  62,  37,  64,  28,  32,
 -13,   7,  28,  17,  31,  24,  32,  -1,
 -31,  -9,   3,  16,  27,   9,  12, -22,
 -49, -23, -14,  -4,  -6,   0, -15, -11,
 -81, -40, -37, -25, -26, -18, -41, -66,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  19,   1,   5,  10,  -3,  25,  -5, -47,
 -13,  10,   7,  14,   4, -19,   9, -28,
 -11,   3,  19,  13, -12, -22,  -9, -29,
  -2,   5,  27,  10,  29,  -5,  11, -26,
  -8,  17,  28,  37,  26,  16, -15, -14,
 -26,  19,  21,  26,  11,   3, -15, -20,
 -40, -15,   7,  14,   4, -10, -22, -59,
 -38, -57, -25,  -9, -24, -39, -55, -81,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -13, -29, -36, -18, -37, -57, -14,  20,
  -7,   3,   6,  -9,   9,   4,  -9, -19,
  -1,  17,  27,  35,  37,  66,  39,  27,
   1,   3,  23,  49,  32,  30,  -1,  -2,
 -13,   9,  11,  28,  21,   4,   1,  -4,
 -18,   5,   7,   8,   7,   4,   5,   1,
 -13,  -8,   4, -13,  -4,   0,  12, -15,
 -37, -26, -31, -29, -23, -36, -23, -27,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  28,  47,  42,  35,  32,  35,  17, -12,
  11,  27,  29,  28,  21,  15,  21, -15,
  15,  20,  14,  10,   1,   4,  14,  -5,
  19,  29,  17,  17,  22,   8,  33,   2,
  15,  28,  32,  25,  32,  30,  19,   5,
  -7,  13,  31,  26,  39,  14,  -6, -20,
 -18,  -1,   1,  22,   8,  -4, -28, -51,
 -10,  13, -14,   0,  -4, -11,  -6,   4,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  50,  47,  43,  38,  35,  36,  47,  40,
  25,  27,  47,  56,  47,  68,  54,  53,
  11,  23,  29,  36,  44,  65,  50,  23,
  -7,  -4,  11,  15,  15,  24,  14,  -4,
 -23, -17, -10,  -3,  -5,  -5,   1, -21,
 -30, -24, -21, -23, -18, -18,   2, -22,
 -34, -27, -16, -20, -18,  -3, -17, -49,
 -13, -14, -10,  -8,  -5,  -3, -24, -21,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  45,  57,  64,  65,  70,  65,  62,  64,
  70,  72,  64,  55,  62,  41,  47,  44,
  72,  65,  63,  58,  48,  39,  42,  57,
  71,  73,  67,  62,  51,  50,  49,  60,
  66,  69,  67,  62,  53,  55,  48,  53,
  42,  44,  46,  40,  40,  35,  21,  34,
  31,  25,  31,  31,  24,  10,  22,  32,
  44,  41,  55,  54,  36,  32,  57,  40,
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
pub static CENTIPAWN_VALUES_MIDGAME: [i16; 7] = [0, 82, 293, 313, 449, 921, 20000];
/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES_ENDGAME: [i16; 7] = [0, 123, 293, 313, 516, 994, 20000];

pub struct EvalFeature {
    pub midgame: i16,
    pub endgame: i16,
}

impl EvalFeature {
    pub const fn new(midgame: i16, endgame: i16) -> Self {
        Self {
            midgame,
            endgame,
        }
    }
}

pub const DOUBLED_PAWN: EvalFeature = EvalFeature::new(17, 29);
pub const PASSED_PAWNS: EvalFeature = EvalFeature::new(6, 13);
pub const ROOK_OPEN_FILES: EvalFeature = EvalFeature::new(37, 2);
pub const ROOF_HALF_OPEN_FILES: EvalFeature = EvalFeature::new(19, 36);
pub const BISHOP_PAIR: EvalFeature = EvalFeature::new(28, 88);
pub const PAWN_SHIELD: i16 = -8;
pub const CONNECTED_PAWNS: EvalFeature = EvalFeature::new(8, 4);
