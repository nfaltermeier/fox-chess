use array_macro::array;

#[rustfmt::skip]
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 188, 158, 146, 138,  90,  44, -31,  34,
  46,  48,  50,  48,  54,  67,  59,  32,
   7,   3,   2,   9,  22,  21,   8,   3,
  -4,  -9,  -3,   5,   3,  16,   1, -12,
 -12, -14, -15, -15,  -7,   0,   9,  -8,
  -7, -11, -17, -18, -13,  15,  26, -13,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
  77,  94,  90,  39,  71, 116, 172, 126,
  87,  82,  49,  19,   9,  19,  40,  42,
  49,  39,  22,  -6,  -6,  -2,  16,   9,
  28,  27,   6, -13,  -9,   0,   9,   0,
  23,  14,  10,   1,   1,   4,  -4,   0,
  35,  22,  18,  -1,  18,   7,  -3,   9,
   0,   0,   0,   0,   0,   0,   0,   0,

];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-115, -15, -25, -11,  18, -23, -17,-157,
 -11,  -1,  24,  39,  29,  33, -22,   8,
  -6,  13,  32,  53,  76,  76,  42,  12,
   2,   8,  31,  48,  26,  46,  25,  35,
  -6,   3,  22,  16,  29,  24,  37,   8,
 -28, -12,  -4,  11,  22,   7,   7, -13,
 -40, -21, -13,   4,   7,  -1, -10,  -3,
 -81, -24, -31, -13, -11,  -9, -13, -66,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  51,  42,  36,  40,  23,  43,  22,  33,
  13,  40,  25,  36,  19,  14,  47, -19,
  28,  28,  57,  41,  18,  20,  16,   5,
  23,  29,  54,  40,  58,  33,  28,  -1,
  20,  31,  59,  61,  44,  45,   6,  15,
   1,  29,  47,  47,  29,  20,   3,   2,
  -5,  17,  11,  15,   8,  -4,  13, -37,
  -7, -16,  11,  13,  -4, -26, -31,  -6,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -12, -29, -19, -21,  -8, -49,  21,   3,
 -18,  -5,  -2,  -1,   8,  -4, -26, -26,
  -6,  12,  20,  23,  32,  63,  40,  25,
 -10,  -1,  16,  43,  21,  25,  -5,  -4,
  -1,  -1,   6,  29,  19,  -4,  -1,   6,
 -13,   9,   8,   7,   7,   6,   8,   0,
  -7,  -5,   8,  -7,   2,   8,  15,  -5,
 -36,  -6, -17, -20, -10, -17, -23, -25,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  45,  67,  51,  49,  30,  44,  21,  14,
  45,  42,  44,  35,  32,  39,  43,  19,
  33,  37,  27,  20,  20,  15,  23,   6,
  32,  42,  29,  30,  41,  23,  49,  27,
  22,  42,  46,  31,  31,  41,  31,  11,
  14,  27,  41,  35,  45,  23,   7,   1,
   7,   9,  11,  31,  11,   8,  -6, -25,
  23,  15,   7,  20,  10,   3,   7,  15,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  39,  42,  38,  37,  35,  39,  47,  40,
  22,  19,  36,  41,  46,  66,  53,  53,
   9,  21,  24,  30,  48,  66,  54,  30,
  -7,  -1,  12,  17,  15,  21,  24,   5,
 -19, -19, -10,  -2,  -2,  -6,   7, -20,
 -29, -18, -21, -17, -11, -13,   1, -25,
 -37, -29, -14, -12, -11,  -3, -13, -50,
 -13, -11,  -6,  -3,   1,   2, -22, -22,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  78,  89,  97,  94, 100,  92,  85,  88,
 106, 109, 104,  99,  93,  76,  84,  79,
 109, 101, 102,  97,  82,  73,  78,  86,
 106, 105, 101,  95,  92,  87,  83,  91,
 100, 107, 104,  94,  89,  93,  81,  86,
  81,  78,  83,  73,  65,  63,  56,  70,
  71,  71,  67,  57,  52,  40,  52,  67,
  61,  55,  68,  60,  40,  42,  67,  65,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   7,  26,  28,  39,  55,  80,  78,  68,
  -5,  -9,  23,  31,  47,  68,  53,  67,
   2,   9,  21,  31,  63,  97,  92,  65,
   2,   6,  17,  25,  40,  39,  48,  36,
   1,   5,  14,  18,  20,  21,  30,  19,
   0,   6,   7,  11,  12,  20,  24,  13,
  -7,   4,  11,  18,  19,  11,  -6, -12,
  -3,  -2,   2,  13,   5, -30, -51, -27,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 136, 134, 156, 163, 162, 108,  99, 107,
 137, 179, 191, 184, 174, 155, 143,  95,
 109, 157, 178, 192, 159, 109,  99, 103,
 121, 147, 165, 179, 165, 176, 162, 142,
 106, 142, 143, 161, 148, 156, 132, 150,
  86, 110, 136, 109, 125, 119, 113,  87,
  88,  78,  89,  54,  57,  41,  50,  46,
  58,  35,  54,  16,  55,   4,  62,  16,
];

#[rustfmt::skip]
/// This appears to have some anomalous values
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -28,  52,  32, -40,  65, -14,  51, -46,
  13,  -2,  54, -15, -38,  37,  -7,  17,
 -28,   8,   0,  -4, -27, -27, -35, -40,
  27,  43,  13,  10,  17,  27,  42,  26,
  16,  39,  36,  34,  30,  26,  10, -11,
   3,  17,  16,  15,  13,  10,   1, -15,
  -3,  -8,  11,  -2,  -5,  -1,  22,  17,
 -46,   9,   5, -41,   8, -29,  24,  10,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -81, -31,   6,  32,  -6,  37,  15,-115,
 -19,  33,  15,  43,  54,  42,  62,   0,
  20,  47,  50,  56,  64,  77,  80,  50,
  -8,  21,  39,  42,  39,  44,  30,   6,
 -32,  -6,  11,  19,  22,  19,   7, -13,
 -47, -18,  -2,  10,  10,   4,  -5, -19,
 -40, -12, -14, -11,  -8,  -9, -27, -53,
 -21, -50, -38, -21, -51, -26, -68, -87,
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
pub static CENTIPAWN_VALUES_MIDGAME: [i16; 7] = [0, 87, 337, 356, 510, 1048, 20000];
/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES_ENDGAME: [i16; 7] = [0, 109, 295, 322, 535, 997, 20000];

pub struct EvalFeature {
    pub midgame: i16,
    pub endgame: i16,
}

impl EvalFeature {
    pub const fn new(midgame: i16, endgame: i16) -> Self {
        Self { midgame, endgame }
    }
}

pub const DOUBLED_PAWN: EvalFeature = EvalFeature::new(-14, -22);
pub const PASSED_PAWNS: EvalFeature = EvalFeature::new(7, 19);
pub const ROOK_OPEN_FILES: EvalFeature = EvalFeature::new(29, -5);
pub const ROOF_HALF_OPEN_FILES: EvalFeature = EvalFeature::new(15, 24);
pub const BISHOP_PAIR: EvalFeature = EvalFeature::new(31, 88);
pub const PAWN_SHIELD: i16 = -9;
pub const CONNECTED_PAWNS: EvalFeature = EvalFeature::new(6, -2);
pub const PIECES_THREATENED_BY_PAWNS: EvalFeature = EvalFeature::new(55, 34);
pub const ISOLATED_PAWN: EvalFeature = EvalFeature::new(-12, -14);

#[rustfmt::skip]
pub static MOBILITY_ROOK_MIDGAME: [i16; 15] = [ -16, -11,  -7,  -4,  -3,   3,   5,   8,  11,  14,  17,  18,  24,  20,  27];
#[rustfmt::skip]
pub static MOBILITY_ROOK_ENDGAME: [i16; 15] = [   0,  -1,  -1,  -2,   4,   5,  12,   7,  13,  15,  17,  19,  14,  14,  -3];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_MIDGAME: [i16; 14] = [ -28, -19, -12,  -7,  -1,   5,   9,  12,  16,  18,  18,  16,  12,  13];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_ENDGAME: [i16; 14] = [   0, -12, -17,  -6,   2,   8,  14,  20,  13,  15,  10,  10,  12,  12];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_MIDGAME: [i16; 9] = [ -41, -19,  -7,  -1,   6,  12,  16,  21,  12];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_ENDGAME: [i16; 9] = [   0,   1,   2,   5,  11,  17,  13,   1, -16];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_MIDGAME: [i16; 14] = [   7,   2,   3,   4,   8,  13,  17,  23,  31,  37,  39,  26,  13,  14];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_ENDGAME: [i16; 14] = [   1,   2,   3,   4,   5,   6,   9,  14,  11,  10,  10,  12,  13,  14];

/// Knight, bishop, rook, queen
pub static KING_ATTACK_UNIT_PIECE_VALUES: [u16; 4] = [6, 36, 81, 57];
