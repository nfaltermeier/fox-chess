use array_macro::array;

use crate::{
    bitboard::{BIT_SQUARES, LIGHT_SQUARES, north_fill, south_fill},
    board::{BISHOP_COLORS_DARK, BISHOP_COLORS_LIGHT, Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK},
    magic_bitboard::{lookup_bishop_attack, lookup_rook_attack},
    moves::Move,
    texel::{EvalParams, FeatureData, FeatureIndex, TaperedFeature},
};

/// Indexed with piece code, so index 0 is no piece
pub static CENTIPAWN_VALUES: [i16; 7] = [0, 82, 292, 313, 445, 903, 20000];

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
  39,  53,  51,  44,  52,  67,  74,  31,
   3,  12,   8,  11,  29,  25,  20,   0,
 -10,   3,  -2,   2,   3,  14,  14, -11,
 -15,  -7, -13, -14,  -3,   3,  19,  -9,
 -13,  -5, -14, -30, -16,  16,  34, -18,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const PAWN_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0,   0,   0,   0,   0,   0,   0,   0,
 189, 200, 201, 152, 179, 199, 249, 217,
 134, 127, 100,  83,  64,  67,  87,  90,
  82,  66,  52,  23,  17,  25,  44,  40,
  57,  54,  28,  14,  17,  24,  31,  22,
  51,  42,  31,  25,  21,  22,  22,  21,
  68,  56,  46,  42,  50,  33,  30,  37,
   0,   0,   0,   0,   0,   0,   0,   0,
];

#[rustfmt::skip]
const KNIGHT_MIDGAME_SQUARE_TABLE: [i16; 64] = [
-124, -21,  -7, -15,  24, -46, -39,-118,
 -10,   4,  22,  40,  36,  38, -33, -10,
   6,  27,  43,  64,  80,  81,  38,   6,
   1,  14,  41,  62,  36,  64,  28,  32,
 -13,   7,  26,  16,  31,  24,  32,  -3,
 -31,  -9,   1,  16,  27,   8,  12, -20,
 -49, -23, -14,  -4,  -5,   0, -15, -16,
 -81, -39, -37, -25, -26, -18, -39, -66,
];

#[rustfmt::skip]
const KNIGHT_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   0, -24, -23, -18, -32,  -1, -26, -60,
 -39, -16, -19, -14, -24, -45, -18, -56,
 -36, -26,  -9, -16, -39, -50, -35, -57,
 -28, -23,  -2, -20,  -1, -32, -18, -55,
 -35, -11,   2,   7,  -4, -14, -44, -43,
 -56, -10,  -5,  -4, -19, -27, -46, -51,
 -66, -47, -26, -19, -27, -41, -52, -89,
 -70, -88, -56, -41, -55, -69, -88,-100,
];

#[rustfmt::skip]
const BISHOP_MIDGAME_SQUARE_TABLE: [i16; 64] = [
 -13, -29, -36, -18, -37, -57, -14,  20,
  -7,   2,   6,  -9,   9,   4,  -8, -19,
  -1,  17,  27,  35,  37,  66,  38,  26,
   2,   4,  23,  49,  33,  30,  -1,  -2,
 -13,  10,  11,  29,  22,   4,   0,  -4,
 -17,   5,   8,   8,   8,   5,   5,   2,
 -13,  -8,   4, -12,  -4,   0,  12, -15,
 -37, -26, -29, -28, -22, -36, -23, -26,
];

#[rustfmt::skip]
const BISHOP_ENDGAME_SQUARE_TABLE: [i16; 64] = [
   2,  20,  17,  11,   8,   9,  -8, -39,
 -15,   4,   5,   4,  -4,  -9,  -4, -39,
  -9,  -3, -10, -14, -23, -19, -11, -29,
  -4,   5,  -7,  -7,  -1, -16,  10, -22,
 -11,   2,   8,  -1,   4,   7,  -4, -20,
 -31, -12,   6,   2,  13, -11, -30, -43,
 -42, -26, -26,  -3, -17, -27, -53, -82,
 -34, -14, -36, -26, -29, -35, -34, -23,
];

#[rustfmt::skip]
const ROOK_MIDGAME_SQUARE_TABLE: [i16; 64] = [
  50,  47,  43,  38,  35,  35,  47,  40,
  25,  28,  47,  56,  47,  68,  54,  53,
  11,  23,  29,  36,  43,  65,  50,  23,
  -6,  -4,  11,  14,  15,  23,  14,  -5,
 -23, -16, -10,  -3,  -5,  -5,   1, -21,
 -30, -24, -20, -23, -18, -18,   1, -22,
 -33, -27, -16, -20, -18,  -3, -18, -49,
 -14, -15, -10,  -8,  -5,  -5, -25, -19,
];

#[rustfmt::skip]
const ROOK_ENDGAME_SQUARE_TABLE: [i16; 64] = [
  64,  75,  79,  79,  86,  85,  83,  87,
  90,  89,  80,  70,  79,  60,  68,  68,
  92,  83,  81,  74,  66,  62,  65,  79,
  91,  91,  83,  77,  69,  72,  69,  82,
  84,  86,  82,  74,  68,  77,  69,  74,
  60,  61,  60,  55,  56,  57,  41,  52,
  48,  44,  47,  46,  42,  33,  40,  46,
  64,  62,  74,  72,  53,  56,  81,  59,
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
  98, 118, 132, 147, 101,  82,  73,  85,
  97, 139, 144, 151, 153,  93, 108,  51,
  74, 110, 122, 135, 117,  61,  46,  30,
  70, 115, 134, 142, 135, 130, 107,  84,
  75, 108, 107, 124, 120, 129,  97,  94,
  59,  63, 106,  91,  87,  94,  89,  62,
  55,  36,  65,  52,  35,   0,  15,  48,
  52,  47,  50,  28,  52,   5,  10,  -5,
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
  13,   1,   9, -14,  -6,  -4,  25,  23,
 -40,  12,   0, -44,  12, -34,  33,  22,
];

#[rustfmt::skip]
const KING_ENDGAME_SQUARE_TABLE: [i16; 64] = [
 -69, -36,  -2,  23, -11,  25,  -1, -64,
  -9,  30,   2,  44,  51,  26,  48,   5,
  19,  38,  48,  51,  59,  72,  77,  47,
  -6,  15,  34,  38,  39,  38,  26,   5,
 -35, -10,  10,  19,  23,  20,  10, -11,
 -47, -22,  -3,   9,  11,   8,  -4, -16,
 -51, -18, -10,  -1,  -1,   0, -19, -44,
  -2, -53, -24, -14, -47, -16, -62, -85,
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

#[inline]
/// for piece_type, pawn is 0
fn get_piece_square_index(color: usize, piece_type: usize, square: usize) -> usize {
    if color == 0 {
        piece_type * 64 + (square ^ 0b00111000)
    } else {
        piece_type * 64 + square
    }
}

impl Board {
    pub fn evaluate(&self, params: &EvalParams) -> i16 {
        self.get_eval_features().evaluate(params)
    }

    pub fn get_eval_features(&self) -> FeatureData {
        let mut result = FeatureData::default();

        let mut piece_counts = [0; 7];
        let mut white_idx = 0;
        let mut black_idx = 0;
        for i in 0..64 {
            let piece = self.get_piece_64(i);
            if piece != PIECE_NONE {
                let color = (piece & COLOR_FLAG_MASK == COLOR_BLACK) as usize;
                let piece_type = (piece & PIECE_MASK) as usize;

                if piece & COLOR_FLAG_MASK == COLOR_BLACK {
                    result.midgame_psqt_black[black_idx] = get_piece_square_index(color, piece_type - 1, i) as u16;
                    result.endgame_psqt_black[black_idx] = get_piece_square_index(color, piece_type - 1 + 6, i) as u16;
                    black_idx += 1;
                } else {
                    result.midgame_psqt_white[white_idx] = get_piece_square_index(color, piece_type - 1, i) as u16;
                    result.endgame_psqt_white[white_idx] = get_piece_square_index(color, piece_type - 1 + 6, i) as u16;
                    white_idx += 1;
                }
                piece_counts[piece_type as usize] += if piece & COLOR_FLAG_MASK == COLOR_BLACK { -1 } else { 1 };
            }
        }

        if self.game_stage > MIN_GAME_STAGE_FULLY_MIDGAME {
            result.game_stage = MIN_GAME_STAGE_FULLY_MIDGAME;
        } else {
            result.game_stage = self.game_stage;
        }

        let mut misc_features_idx = 0;
        for (i, c) in piece_counts.iter().enumerate() {
            if *c != 0 {
                result.misc_features[misc_features_idx] = (*c, FeatureIndex::PieceValues + i as u16);
                misc_features_idx += 1;
            }
        }

        let doubled_pawns = self.count_doubled_pawns();

        let white_passed = self.white_passed_pawns();
        let white_passed_distance = (south_fill(white_passed) & !white_passed).count_ones() as i16;

        let black_passed = self.black_passed_pawns();
        let black_passed_distance = (north_fill(black_passed) & !black_passed).count_ones() as i16;

        let net_passed_pawns = white_passed_distance - black_passed_distance;
        let net_connected_pawns = self.get_connected_pawns(true).count_ones() as i16 - self.get_connected_pawns(false).count_ones() as i16;

        let (w_open, w_half_open) = self.rooks_on_open_files(true);
        let (b_open, b_half_open) = self.rooks_on_open_files(false);
        let net_rooks_on_open_files = w_open - b_open;
        let net_rooks_on_half_open_files = w_half_open - b_half_open;

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

        let game_stage_for_pawn_shield = if self.game_stage <= ENDGAME_GAME_STAGE_FOR_QUIESCENSE {
            0
        } else {
            self.game_stage - ENDGAME_GAME_STAGE_FOR_QUIESCENSE
        };
        // How much pawn shield each side is missing. Positive: white is missing more
        let net_pawn_shield_penalty = (6 - self.score_pawn_shield(0)) - (6 - self.score_pawn_shield(1));
        
        result.pawn_shield = TaperedFeature {
            weight: net_pawn_shield_penalty,
            idx: FeatureIndex::PawnShield as u16,
            taper_amount: game_stage_for_pawn_shield,
            max_amount: MAX_GAME_STAGE - ENDGAME_GAME_STAGE_FOR_QUIESCENSE,
        };

        if doubled_pawns != 0 {
            result.misc_features[misc_features_idx] = (doubled_pawns as i8, FeatureIndex::DoubledPawns as u16);
            misc_features_idx += 1;
        }
        if net_passed_pawns != 0 {
            result.misc_features[misc_features_idx] = (net_passed_pawns as i8, FeatureIndex::PassedPawns as u16);
            misc_features_idx += 1;
        }
        if net_rooks_on_open_files != 0 {
            result.misc_features[misc_features_idx] = (net_rooks_on_open_files as i8, FeatureIndex::RookOpenFile as u16);
            misc_features_idx += 1;
        }
        if net_rooks_on_half_open_files != 0 {
            result.misc_features[misc_features_idx] = (net_rooks_on_half_open_files as i8, FeatureIndex::RookHalfOpenFile as u16);
            misc_features_idx += 1;
        }
        if bishop_pair != 0 {
            result.misc_features[misc_features_idx] = (bishop_pair as i8, FeatureIndex::BishopPair as u16);
            misc_features_idx += 1;
        }
        if net_connected_pawns != 0 {
            result.misc_features[misc_features_idx] = (net_connected_pawns as i8, FeatureIndex::ConnectedPawns as u16);
            misc_features_idx += 1;
        }

        result
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
