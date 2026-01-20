use crate::{
    bitboard::{
        BIT_SQUARES, LIGHT_SQUARES, bitscan_forward_and_reset, generate_pawn_attack, lookup_knight_attack, north_fill,
        south_fill,
    },
    board::{
        BISHOP_COLORS_DARK, BISHOP_COLORS_LIGHT, Board, COLOR_BLACK, COLOR_FLAG_MASK, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK
    },
    eval_values::{
        BISHOP_PAIR, CENTIPAWN_VALUES_ENDGAME, CENTIPAWN_VALUES_MIDGAME, CONNECTED_PAWNS, DOUBLED_PAWN, ISOLATED_PAWN,
        MOBILITY_BISHOP_ENDGAME, MOBILITY_BISHOP_MIDGAME, MOBILITY_KNIGHT_ENDGAME, MOBILITY_KNIGHT_MIDGAME,
        MOBILITY_ROOK_ENDGAME, MOBILITY_ROOK_MIDGAME, PASSED_PAWNS, PAWN_SHIELD, PIECES_THREATENED_BY_PAWNS,
        ROOF_HALF_OPEN_FILES, ROOK_OPEN_FILES,
    },
    magic_bitboard::{lookup_bishop_attack, lookup_rook_attack},
    moves::Move, texel::{EvalParams, FeatureData, FeatureIndex, TaperedFeature},
};

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
                result.misc_features[misc_features_idx] = (*c, FeatureIndex::PieceValues + i as u16 * 2);
                misc_features_idx += 1;
            }
        }

        let (doubled_pawns, isolated_pawns) = self.count_doubled_isolated_pawns();

        let white_passed = self.white_passed_pawns();
        let white_passed_distance = (south_fill(white_passed) & !white_passed).count_ones() as i16;

        let black_passed = self.black_passed_pawns();
        let black_passed_distance = (north_fill(black_passed) & !black_passed).count_ones() as i16;

        let net_passed_pawns = white_passed_distance - black_passed_distance;
        let net_connected_pawns =
            self.get_connected_pawns(true).count_ones() as i16 - self.get_connected_pawns(false).count_ones() as i16;

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

        let net_pieces_threatened_by_pawns = self.get_pieces_threatened_by_pawns(true).count_ones() as i16 - self.get_pieces_threatened_by_pawns(false).count_ones() as i16;

        self.calculate_mobility(&mut result, &mut misc_features_idx);

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
        if net_pieces_threatened_by_pawns != 0 {
            result.misc_features[misc_features_idx] = (net_pieces_threatened_by_pawns as i8, FeatureIndex::PawnsThreatenPieces as u16);
            misc_features_idx += 1;
        }
        if isolated_pawns != 0 {
            result.misc_features[misc_features_idx] = (isolated_pawns as i8, FeatureIndex::IsolatedPawns as u16);
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
        values[0] = CENTIPAWN_VALUES_MIDGAME[(self.get_piece_64(to as usize) & PIECE_MASK) as usize];
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
            values[depth] = CENTIPAWN_VALUES_MIDGAME[last_attacker] - values[depth - 1];
            depth += 1;
            color = if color != 0 { 0 } else { 1 };
            last_attacker = next_attacker_piece;
        }

        for i in (1..depth).rev() {
            values[i - 1] = -values[i].max(-values[i - 1]);
        }

        values[0]
    }

    // Based on the code from https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm
    /// Move is expected to be a capture but probably will work if it isn't. En passant, castling, and promotions are not supported.
    pub fn is_static_exchange_eval_at_least(&self, m: Move, threshold: i16) -> bool {
        let from = m.from();
        let to = m.to();

        let mut values = [0; 32];
        values[0] = CENTIPAWN_VALUES_MIDGAME[(self.get_piece_64(to as usize) & PIECE_MASK) as usize];
        let mut last_attacker = (self.get_piece_64(from as usize) & PIECE_MASK) as usize;

        // If losing the attacker with no followup is greater than the threshold, then no need to investigate further
        if values[0] - CENTIPAWN_VALUES_MIDGAME[last_attacker] >= threshold {
            return true;
        }

        let mut occupancy = self.occupancy & !BIT_SQUARES[from as usize];
        let mut attacks_data = self.get_attacks_to(to as u8, occupancy);
        attacks_data.attackers &= !BIT_SQUARES[from as usize];

        let mut depth = 1;
        let mut color = if self.white_to_move { 1 } else { 0 };

        loop {
            // Check if the last move opened up an x-ray
            if attacks_data.possible_rook_like_x_rays != 0
                && (last_attacker == PIECE_ROOK as usize || last_attacker == PIECE_QUEEN as usize)
            {
                let new_attacks = lookup_rook_attack(to as u8, occupancy) & attacks_data.possible_rook_like_x_rays;
                attacks_data.attackers |= new_attacks;
                attacks_data.possible_rook_like_x_rays ^= new_attacks;
            }

            if attacks_data.possible_bishop_like_x_rays != 0
                && (last_attacker == PIECE_BISHOP as usize
                    || last_attacker == PIECE_QUEEN as usize
                    || last_attacker == PIECE_PAWN as usize)
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
            values[depth] = CENTIPAWN_VALUES_MIDGAME[last_attacker] - values[depth - 1];

            // If the other side just moved and their value is less than the negative threshold value,
            // then break to ensure that they wouldn't stop earlier
            if depth % 2 == 1 && values[depth].max(-values[depth - 1]) <= -threshold {
                break;
            }

            depth += 1;
            color = if color != 0 { 0 } else { 1 };
            last_attacker = next_attacker_piece;
        }

        for i in (1..depth).rev() {
            values[i - 1] = -values[i].max(-values[i - 1]);
        }

        values[0] >= threshold
    }

    fn calculate_mobility(&self, features: &mut FeatureData, misc_features_idx: &mut usize) {
        let not_other_side_pawn_guarded = [
            !generate_pawn_attack(self.piece_bitboards[1][PIECE_PAWN as usize], false),
            !generate_pawn_attack(self.piece_bitboards[0][PIECE_PAWN as usize], true),
        ];
        let not_own_pieces = [!self.side_occupancy[0], !self.side_occupancy[1]];

        let mut net_rook_mobility_values = [0; 15];
        for side in 0..=1 {
            let side_value_mult = if side == 0 { 1 } else { -1 };
            let mut rooks = self.piece_bitboards[side][PIECE_ROOK as usize];
            while rooks != 0 {
                let rook = bitscan_forward_and_reset(&mut rooks) as u8;
                let squares = lookup_rook_attack(rook, self.occupancy);
                let mobility = (squares & not_other_side_pawn_guarded[side] & not_own_pieces[side]).count_ones();

                net_rook_mobility_values[mobility as usize] += side_value_mult;
            }
        }

        for (i, v) in net_rook_mobility_values.iter().enumerate() {
            if *v != 0 {
                features.misc_features[*misc_features_idx] = (*v, FeatureIndex::RookMobility as u16 + 2 * (i as u16));
                *misc_features_idx += 1;
            }
        }

        let mut net_bishop_mobility_values = [0; 14];
        for side in 0..=1 {
            let side_value_mult = if side == 0 { 1 } else { -1 };
            let mut bishops = self.piece_bitboards[side][PIECE_BISHOP as usize];
            while bishops != 0 {
                let bishop = bitscan_forward_and_reset(&mut bishops) as u8;
                let squares = lookup_bishop_attack(bishop, self.occupancy);
                let mobility = (squares & not_other_side_pawn_guarded[side] & not_own_pieces[side]).count_ones();

                net_bishop_mobility_values[mobility as usize] += side_value_mult;
            }
        }

        for (i, v) in net_bishop_mobility_values.iter().enumerate() {
            if *v != 0 {
                features.misc_features[*misc_features_idx] = (*v, FeatureIndex::BishopMobility as u16 + 2 * (i as u16));
                *misc_features_idx += 1;
            }
        }

        let mut net_knight_mobility_values = [0; 9];
        for side in 0..=1 {
            let side_value_mult = if side == 0 { 1 } else { -1 };
            let mut knights = self.piece_bitboards[side][PIECE_KNIGHT as usize];
            while knights != 0 {
                let knight = bitscan_forward_and_reset(&mut knights) as u8;
                let squares = lookup_knight_attack(knight);
                let mobility = (squares & not_other_side_pawn_guarded[side] & not_own_pieces[side]).count_ones();

                net_knight_mobility_values[mobility as usize] += side_value_mult;
            }
        }

        for (i, v) in net_knight_mobility_values.iter().enumerate() {
            if *v != 0 {
                features.misc_features[*misc_features_idx] = (*v, FeatureIndex::KnightMobility as u16 + 2 * (i as u16));
                *misc_features_idx += 1;
            }
        }
    }
}

#[cfg(test)]
mod eval_tests {
    use crate::{STARTING_FEN, texel::DEFAULT_PARAMS};
    use crate::magic_bitboard::initialize_magic_bitboards;

    use super::*;

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

    macro_rules! see_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (fen, expected_eval, m_str) = $value;

                    let board = Board::from_fen(fen).unwrap();

                    initialize_magic_bitboards();

                    let m = Move::from_simple_long_algebraic_notation(m_str, 0);
                    let see_result = board.static_exchange_eval(m);

                    assert_eq!(expected_eval, see_result);
                    assert!(board.is_static_exchange_eval_at_least(m, expected_eval - 1));
                    assert!(board.is_static_exchange_eval_at_least(m, expected_eval));
                    assert!(!board.is_static_exchange_eval_at_least(m, expected_eval + 1));
                }
            )*
        }
    }

    see_test! {
        // Some positions taken from https://github.com/zzzzz151/Starzix/blob/main/tests/SEE.txt
        no_recapture: ("1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - -", CENTIPAWN_VALUES_MIDGAME[PIECE_PAWN as usize], "e1e5"),
        pawn_captures: ("k7/8/4p1p1/5p2/4P1P1/8/8/K7 w - - 0 1", 0, "e4f5"),
        sliders_behind_capturing_piece: ("2r2r1k/6bp/p7/2q2p1Q/3PpP2/1B6/P5PP/2RR3K b - -", CENTIPAWN_VALUES_MIDGAME[PIECE_ROOK as usize] * 2 - CENTIPAWN_VALUES_MIDGAME[PIECE_QUEEN as usize], "c5c1"),
        pawn_before_rook: ("4R3/2r3p1/5bk1/1p1r1p1p/p2PR1P1/P1BK1P2/1P6/8 b - -", 0, "h5g4"),
        bishop_for_knight_no_losing_queen_capture: ("5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - -", -CENTIPAWN_VALUES_MIDGAME[PIECE_BISHOP as usize] + CENTIPAWN_VALUES_MIDGAME[PIECE_KNIGHT as usize], "d6f4"),
        non_capture1: ("2r1k2r/pb4pp/5p1b/2KB3n/4N3/2NP1PB1/PPP1P1PP/R2Q3R w k -", -CENTIPAWN_VALUES_MIDGAME[PIECE_BISHOP as usize], "d5c6"),
        non_capture1_recapture: ("2r1k2r/pb4pp/5p1b/2KB3n/1N2N3/3P1PB1/PPP1P1PP/R2Q3R w k -", 0, "d5c6"),
        rook_xray: ("4q3/1p1pr1k1/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -", CENTIPAWN_VALUES_MIDGAME[PIECE_PAWN as usize] - CENTIPAWN_VALUES_MIDGAME[PIECE_ROOK as usize], "e6e4"),
        rook_xray_extra_defender: ("4q3/1p1pr1kb/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -", CENTIPAWN_VALUES_MIDGAME[PIECE_PAWN as usize], "e6e4"),
        // I think the best is if everything gets traded off, this is the net change of that. It fails, not sure if that is because my bishop val != knight val
        // big_trade_both_xrays: ("3r3k/3r4/2n1n3/8/3p4/2PR4/1B1Q4/3R3K w - -", CENTIPAWN_VALUES_MIDGAME[PIECE_KNIGHT as usize] * 2 - CENTIPAWN_VALUES_MIDGAME[PIECE_BISHOP as usize] + CENTIPAWN_VALUES_MIDGAME[PIECE_ROOK as usize] - CENTIPAWN_VALUES_MIDGAME[PIECE_QUEEN as usize], "d3d4"),
        bench_is_at_least: ("3r1rk1/ppp1pp1p/6p1/3qb2P/3n4/4BN2/PP2BP2/R2Q1RK1 w - - 0 16", -CENTIPAWN_VALUES_MIDGAME[PIECE_QUEEN as usize] + CENTIPAWN_VALUES_MIDGAME[PIECE_BISHOP as usize] + CENTIPAWN_VALUES_MIDGAME[PIECE_KNIGHT as usize], "d1d4"),
    }
}
