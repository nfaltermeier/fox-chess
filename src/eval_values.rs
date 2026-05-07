use array_macro::array;

// Not currently part of eval tuning, so keeping it up here to separate it a bit
pub const TEMPO_BONUS: i16 = 10;

#[rustfmt::skip]
const PAWN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 194, 161, 154, 141, 103,  64,  -2,  43,
  41,  52,  53,  60,  64,  84,  83,  38,
  10,   3,   5,   8,  25,  29,  10,   2,
  -4, -11,  -3,   3,   7,  18,   5,  -9,
 -15, -17, -16, -19, -10,   4,   9,  -8,
  -6,  -9, -14, -19, -18,  19,  26, -10,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
  64,  94,  87,  35,  60, 103, 172, 124,
  86,  75,  52,  13,  12,  21,  44,  45,
  47,  44,  28,  -4,  -1,   2,  21,  20,
  24,  29,   9,  -9, -10,   1,  10,   5,
  19,  15,  13,  -3,   5,   5,  -5,   3,
  31,  23,  18,   5,  18,   8,   1,  12,
   0,   0,   0,   0,   0,   0,   0,   0,

];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-140, -37, -21,  26,  32,  15,   9,-104,
  -9,   0,  23,  53,  43,  40,  16,  21,
  -7,  16,  42,  51,  78,  82,  43,  17,
   2,   4,  33,  54,  34,  51,  28,  40,
 -13,   2,  25,  25,  34,  27,  44,   9,
 -41, -17,  -8,  16,  21,   7,   5, -13,
 -40, -24, -25,   3,   5,  -2,  -8,  -6,
 -81, -30, -31, -16, -11, -15, -20, -65,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  35,  46,  35,  24,   1,  34,  29,  27,
  15,  33,  17,  12,  15,   9,  11, -24,
  16,  31,  50,  49,  23,   2,  16,  12,
  17,  40,  56,  44,  64,  42,  29,  -1,
  26,  32,  53,  59,  49,  48,  -3,  28,
 -12,  25,  48,  46,  36,  22,  12, -10,
 -17,  12,  16,  22,  10,   4,  26, -37,
 -17, -39,  -2,   6, -13, -17, -18, -19,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -27, -30, -19, -17,   6, -31,   1, -16,
 -20,  -6,   3,   5,  14,  -1,  -7, -11,
  -6,  15,  15,  31,  35,  63,  38,  24,
 -11,   4,  14,  47,  26,  29,   0,  -8,
  -5,  -7,   7,  31,  24,  -6,  -1,   8,
 -17,  10,   7,  10,   6,   6,   8,   4,
  -5,  -4,  11,  -9,   0,   0,  13,   0,
 -30,   0, -17, -21, -16, -18, -23, -21,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  38,  59,  52,  43,  24,  32,  26,  22,
  29,  41,  36,  29,  23,  37,  31,  18,
  27,  36,  24,   7,  18,  -3,  13,   1,
  25,  35,  23,  25,  43,  25,  36,  23,
  17,  30,  42,  29,  26,  41,  28,  -9,
  11,  21,  32,  26,  41,  17,   7, -14,
  -4,   0,   3,  21,  12,   4,  -8, -22,
   5,  -5,   5,  16,   7,   1, -11,   2,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  39,  41,  38,  37,  35,  39,  47,  43,
  24,  24,  37,  42,  46,  65,  53,  53,
  11,  28,  29,  31,  49,  66,  60,  30,
  -6,   1,  15,  25,  16,  21,  24,   5,
 -23, -18, -10,  -3,   1, -16,   6, -25,
 -34, -22, -20, -20, -13, -16,   1, -23,
 -38, -26, -21, -19, -15, -10, -13, -50,
 -13, -10,  -7,  -6,  -2,   2,  -9, -21,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  76,  93,  98, 103, 105, 105,  91,  87,
 104, 104,  99, 100, 106,  87,  85,  82,
 109, 100,  99,  98,  85,  83,  71,  90,
 107, 105,  97,  90,  96,  94,  86,  98,
 100, 103, 101,  90,  88,  99,  92,  95,
  78,  82,  78,  72,  64,  67,  64,  68,
  63,  62,  63,  61,  51,  49,  59,  62,
  60,  50,  59,  56,  38,  41,  44,  67,
];

#[rustfmt::skip]
const QUEEN_MIDGAME_SQUARE_TABLE: [i16; 64] = [
   5,  32,  32,  49,  57,  79,  80,  73,
  -4, -10,  24,  32,  50,  66,  53,  73,
   2,  10,  17,  37,  57,  97,  90,  68,
   1,   6,  19,  24,  45,  46,  54,  35,
  -3,  10,  11,  21,  22,  27,  33,  27,
  -8,   3,  10,   7,  10,  18,  28,  18,
 -14,   1,  10,  15,  15,  10,  -7,  -6,
   0,  -4,   3,  10,   2, -30, -50, -27,
];

#[rustfmt::skip]
const QUEEN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 145, 152, 171, 160, 149, 114, 100, 120,
 129, 175, 177, 189, 184, 152, 128,  80,
 117, 157, 171, 169, 169, 141,  77, 105,
 108, 159, 169, 182, 170, 182, 150, 149,
 100, 113, 140, 152, 149, 139, 120, 149,
  57,  95, 119, 101, 111, 106,  76,  58,
  70,  65,  71,  53,  40,  42,   9,   0,
  39,  22,   4,  28,  10,   2,  -7, -13,
];

#[rustfmt::skip]
/// This appears to have some anomalous values
const KING_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -28,  52,  32, -40,  65, -14,  51, -46,
  13,  -2,  54, -15, -38,  37,  -7,  17,
 -28,   8,   0,  -4, -27, -26, -35, -40,
  27,  44,  13,  10,  17,  27,  42,  25,
  16,  39,  36,  34,  32,  25,  10, -11,
   3,  17,  17,  14,   6,  10,  -2, -15,
   8,  -9,  11,  -1, -11,  -3,  21,  15,
 -46,   9,  10, -38,  -4, -34,  24,   6,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -94, -31,   5,  32,  -4,  33,   0,-112,
 -29,  44,  25,  41,  61,  36,  48, -15,
  26,  59,  59,  58,  68,  78,  72,  54,
  -4,  23,  41,  47,  48,  44,  29,  -1,
 -27,  -3,  17,  25,  23,  18,  11, -11,
 -42, -17,   2,  12,  17,   4,  -5, -28,
 -37, -10, -14,  -9,  -5, -12, -30, -56,
 -25, -52, -43, -32, -51, -27, -72, -94,
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
pub static CENTIPAWN_VALUES_MIDGAME: [i16; 7] = [0, 97, 359, 378, 544, 1097, 20000];
/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES_ENDGAME: [i16; 7] = [0, 105, 289, 324, 530, 997, 20000];

pub struct EvalFeature {
    pub midgame: i16,
    pub endgame: i16,
}

impl EvalFeature {
    pub const fn new(midgame: i16, endgame: i16) -> Self {
        Self { midgame, endgame }
    }
}

pub const DOUBLED_PAWN: EvalFeature = EvalFeature::new(-18, -19);
pub const PASSED_PAWNS: EvalFeature = EvalFeature::new(7, 19);
pub const ROOK_OPEN_FILES: EvalFeature = EvalFeature::new(35, -9);
pub const ROOF_HALF_OPEN_FILES: EvalFeature = EvalFeature::new(19, 20);
pub const BISHOP_PAIR: EvalFeature = EvalFeature::new(38, 88);
pub const PAWN_SHIELD: i16 = -7;
pub const CONNECTED_PAWNS: EvalFeature = EvalFeature::new(7, -3);
pub const PIECES_THREATENED_BY_PAWNS: EvalFeature = EvalFeature::new(55, 34);
pub const ISOLATED_PAWN: EvalFeature = EvalFeature::new(-12, -16);

#[rustfmt::skip]
pub static MOBILITY_ROOK_MIDGAME: [i16; 15] = [ -23, -12, -10,  -5,  -5,   2,   4,   9,  12,  17,  20,  22,  25,  23,  30];
#[rustfmt::skip]
pub static MOBILITY_ROOK_ENDGAME: [i16; 15] = [   0,  -1,  -2,  -3,   4,   4,  12,   8,  16,  18,  18,  19,  14,  14,  -3];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_MIDGAME: [i16; 14] = [ -40, -28, -14,  -8,   0,   8,  12,  14,  19,  20,  21,  19,  12,  13];
#[rustfmt::skip]
pub static MOBILITY_BISHOP_ENDGAME: [i16; 14] = [   0, -14, -20,  -7,   2,   8,  13,  20,  14,  17,  10,  10,  12,  12];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_MIDGAME: [i16; 9] = [ -43, -22,  -9,   0,   8,  15,  21,  25,  17];
#[rustfmt::skip]
pub static MOBILITY_KNIGHT_ENDGAME: [i16; 9] = [   0,   1,   2,   7,  12,  18,  13,  -1, -16];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_MIDGAME: [i16; 14] = [   2,  -1,   1,   5,  12,  18,  23,  27,  34,  39,  39,  27,  13,  14];
#[rustfmt::skip]
pub static MOBILITY_QUEEN_ENDGAME: [i16; 14] = [   1,   2,   3,   4,   5,   6,   9,  14,  12,  10,   9,  12,  13,  14];

/// Knight, bishop, rook, queen
pub static KING_ATTACK_UNIT_PIECE_VALUES: [u16; 4] = [14, 37, 77, 62];
