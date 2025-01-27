use log::{debug, error, log_enabled, trace};

use crate::{
    bitboard::{bitscan_forward_and_reset, lookup_pawn_attack, BIT_SQUARES},
    board::{
        index_10x12_to_pos_str, piece_to_name, Board, BOARD_SQUARE_INDEX_TRANSLATION_64, CASTLE_BLACK_KING_FLAG,
        CASTLE_BLACK_QUEEN_FLAG, CASTLE_WHITE_KING_FLAG, CASTLE_WHITE_QUEEN_FLAG, COLOR_BLACK, COLOR_FLAG_MASK,
        DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION, HASH_VALUES, PIECE_BISHOP, PIECE_INVALID, PIECE_KING,
        PIECE_KNIGHT, PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK,
    },
    evaluate::CENTIPAWN_VALUES,
    moves::{
        Move, MoveRollback, MOVE_DOUBLE_PAWN, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_PROMOTION,
        MOVE_KING_CASTLE, MOVE_PROMO_BISHOP, MOVE_PROMO_KNIGHT, MOVE_PROMO_QUEEN, MOVE_PROMO_ROOK, MOVE_QUEEN_CASTLE,
    },
    search::{HistoryTable, DEFAULT_HISTORY_TABLE},
};

const ENABLE_PERFT_STATS: bool = true;
/// This option is slow
const ENABLE_PERFT_STATS_CHECKS: bool = false;
/// This option is very slow
const ENABLE_PERFT_STATS_CHECKMATES: bool = false;
pub const ENABLE_UNMAKE_MOVE_TEST: bool = false;

/// Has value of target - self added so typical range is +-800. I guess kings capturing have the highest value.
/// Capturing promotions also have value of piece to become added so their additional range is +300 to +1800
const MOVE_SCORE_CAPTURE: i16 = 2000;
pub const MOVE_SCORE_KILLER_1: i16 = 1999;
pub const MOVE_SCORE_KILLER_2: i16 = 1998;
/// Has value of piece is becomes added so really the range is +300 to +900
const MOVE_SCORE_PROMOTION: i16 = 1000;
const MOVE_SCORE_KING_CASTLE: i16 = 502;
const MOVE_SCORE_QUEEN_CASTLE: i16 = 501;
/// No idea what a good value is; only applied to quiet moves. Can also go down to negative this value.
pub const MOVE_SCORE_HISTORY_MAX: i32 = 500;
const MOVE_SCORE_QUIET: i16 = 0;

/// Values from https://www.chessprogramming.org/10x12_Board under TSCP
/// If the piece can slide through squares when moving
const SLIDES: [bool; 5] = [false, true, true, true, false];
#[rustfmt::skip]
/// 10x12 repr offsets for moving in each piece's valid directions
const OFFSET: [[i8; 8]; 5] = [
	[ -21, -19,-12, -8, 8, 12, 19, 21 ], /* KNIGHT */
	[ -11,  -9,  9, 11, 0,  0,  0,  0 ], /* BISHOP */
	[ -10,  -1,  1, 10, 0,  0,  0,  0 ], /* ROOK */
	[ -11, -10, -9, -1, 1,  9, 10, 11 ], /* QUEEN */
	[ -11, -10, -9, -1, 1,  9, 10, 11 ]  /* KING */
];

impl Board {
    #[inline]
    pub fn generate_pseudo_legal_moves_with_history(&mut self, history_table: &HistoryTable) -> Vec<ScoredMove> {
        let moves = self.generate_moves_pseudo_legal::<true, false>(history_table);

        self.do_make_unmake_move_test(&moves);

        moves
    }

    #[inline]
    pub fn generate_legal_moves_without_history(&mut self) -> Vec<ScoredMove> {
        self.generate_legal_moves::<false, false>(&DEFAULT_HISTORY_TABLE)
    }

    #[inline]
    pub fn generate_pseudo_legal_moves_without_history(&mut self) -> Vec<ScoredMove> {
        let moves = self.generate_moves_pseudo_legal::<false, false>(&DEFAULT_HISTORY_TABLE);

        self.do_make_unmake_move_test(&moves);

        moves
    }

    #[inline]
    pub fn generate_pseudo_legal_capture_moves(&mut self) -> Vec<ScoredMove> {
        let moves = self.generate_moves_pseudo_legal::<false, true>(&DEFAULT_HISTORY_TABLE);

        self.do_make_unmake_move_test(&moves);

        moves
    }

    /// if make and unmake move work properly then at the end board should be back to it's original state
    pub fn generate_legal_moves<const USE_HISTORY: bool, const ONLY_CAPTURES: bool>(
        &mut self,
        history_table: &HistoryTable,
    ) -> Vec<ScoredMove> {
        let mut moves = self.generate_moves_pseudo_legal::<USE_HISTORY, ONLY_CAPTURES>(history_table);
        let mut rollback = MoveRollback::default();
        let mut board_copy = None;
        if ENABLE_UNMAKE_MOVE_TEST {
            board_copy = Some(self.clone());
        }

        // if log_enabled!(log::Level::Trace) {
        //     for m in &moves {
        //         trace!("{}", m.pretty_print(Some(self)));
        //     }
        // }

        moves.retain(|r#move| {
            let (result, move_made) = self.test_legality_and_maybe_make_move(r#move.m, &mut rollback);

            if move_made {
                self.unmake_move(&r#move.m, &mut rollback);

                if ENABLE_UNMAKE_MOVE_TEST && board_copy.as_ref().unwrap() != self {
                    error!("unmake move did not properly undo move {:?}", r#move.m);

                    let board_copied = board_copy.as_ref().unwrap();
                    if board_copied.hash != self.hash {
                        for (i, v) in HASH_VALUES.iter().enumerate() {
                            if board_copied.hash ^ v == self.hash {
                                debug!("make/unmake differs by value {i} of HASH_VALUES in hash");
                            }
                        }
                    }

                    assert_eq!(board_copy.as_ref().unwrap(), self);
                }
            }

            result
        });

        debug_assert!(rollback.is_empty());

        moves
    }

    pub fn generate_moves_pseudo_legal<const USE_HISTORY: bool, const ONLY_CAPTURES: bool>(
        &self,
        history_table: &HistoryTable,
    ) -> Vec<ScoredMove> {
        let mut result = Vec::new();
        let side = if self.white_to_move { 0 } else { 1 };
        let other_side = if self.white_to_move { 1 } else { 0 };

        // Pawn move generation TODO: setwise pawn attacks
        let mut pieces = self.piece_bitboards[side][PIECE_PAWN as usize];
        while pieces != 0 {
            let from = bitscan_forward_and_reset(&mut pieces) as u8;
            let promo =
                (self.white_to_move && from >= 48 && from <= 55) || (!self.white_to_move && from >= 8 && from <= 15);

            let mut attacks = lookup_pawn_attack(from, self.white_to_move) & self.side_occupancy[other_side];
            while attacks != 0 {
                let to = bitscan_forward_and_reset(&mut attacks) as u8;
                let target_piece = self.get_piece_64(to as usize);

                if !promo {
                    result.push(ScoredMove {
                        m: Move::new(from, to, MOVE_FLAG_CAPTURE),
                        score: MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES[(target_piece & PIECE_MASK) as usize]
                            - CENTIPAWN_VALUES[PIECE_PAWN as usize],
                    });
                } else {
                    let score = MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES[(target_piece & PIECE_MASK) as usize]
                        - CENTIPAWN_VALUES[PIECE_PAWN as usize];
                    result.push(ScoredMove::new(
                        from,
                        to,
                        MOVE_PROMO_QUEEN | MOVE_FLAG_CAPTURE,
                        score + CENTIPAWN_VALUES[PIECE_QUEEN as usize],
                    ));
                    result.push(ScoredMove::new(
                        from,
                        to,
                        MOVE_PROMO_ROOK | MOVE_FLAG_CAPTURE,
                        score + CENTIPAWN_VALUES[PIECE_ROOK as usize],
                    ));
                    result.push(ScoredMove::new(
                        from,
                        to,
                        MOVE_PROMO_BISHOP | MOVE_FLAG_CAPTURE,
                        score + CENTIPAWN_VALUES[PIECE_BISHOP as usize],
                    ));
                    result.push(ScoredMove::new(
                        from,
                        to,
                        MOVE_PROMO_KNIGHT | MOVE_FLAG_CAPTURE,
                        score + CENTIPAWN_VALUES[PIECE_KNIGHT as usize],
                    ));
                }
            }

            if !ONLY_CAPTURES {
                let from_bitsquare = BIT_SQUARES[from as usize];
                let to_bitsquare = if self.white_to_move {
                    from_bitsquare << 8
                } else {
                    from_bitsquare >> 8
                };

                if to_bitsquare & self.occupancy == 0 {
                    let to = if self.white_to_move { from + 8 } else { from - 8 };
                    if promo {
                        result.push(ScoredMove::new(
                            from,
                            to,
                            MOVE_PROMO_QUEEN,
                            MOVE_SCORE_PROMOTION + CENTIPAWN_VALUES[PIECE_QUEEN as usize],
                        ));
                        result.push(ScoredMove::new(
                            from,
                            to,
                            MOVE_PROMO_ROOK,
                            MOVE_SCORE_PROMOTION + CENTIPAWN_VALUES[PIECE_ROOK as usize],
                        ));
                        result.push(ScoredMove::new(
                            from,
                            to,
                            MOVE_PROMO_BISHOP,
                            MOVE_SCORE_PROMOTION + CENTIPAWN_VALUES[PIECE_BISHOP as usize],
                        ));
                        result.push(ScoredMove::new(
                            from,
                            to,
                            MOVE_PROMO_KNIGHT,
                            MOVE_SCORE_PROMOTION + CENTIPAWN_VALUES[PIECE_KNIGHT as usize],
                        ));
                    } else {
                        result.push(ScoredMove::new(
                            from,
                            to,
                            0,
                            MOVE_SCORE_QUIET
                                + if USE_HISTORY {
                                    history_table[side][PIECE_PAWN as usize - 1][to as usize]
                                } else {
                                    0
                                },
                        ));
                    }

                    // double move
                    if (!self.white_to_move && from >= 48 && from <= 55)
                        || (self.white_to_move && from >= 8 && from <= 15)
                    {
                        let doublemove_bitsquare = if self.white_to_move {
                            to_bitsquare << 8
                        } else {
                            to_bitsquare >> 8
                        };
                        let doublemove_to = if self.white_to_move { to + 8 } else { to - 8 };

                        if doublemove_bitsquare & self.occupancy == 0 {
                            result.push(ScoredMove::new(
                                from,
                                doublemove_to,
                                MOVE_DOUBLE_PAWN,
                                MOVE_SCORE_QUIET
                                    + if USE_HISTORY {
                                        history_table[side][PIECE_PAWN as usize - 1]
                                            [doublemove_to as usize]
                                    } else {
                                        0
                                    },
                            ));
                        }
                    }
                }
            }

            match self.en_passant_target_square_index {
                Some(ep_target_64) => {
                    let potential_takers = lookup_pawn_attack(ep_target_64, !self.white_to_move);
                    let mut takers = potential_takers & self.piece_bitboards[side][PIECE_PAWN as usize];
                    while takers != 0 {
                        let from = bitscan_forward_and_reset(&mut takers) as u8;

                        result.push(ScoredMove {
                            m: Move::new(from, ep_target_64, MOVE_EP_CAPTURE),
                            score: MOVE_SCORE_CAPTURE,
                        });
                    }
                },
                None => {},
            }
        }

        result
    }

    /// If the move is legal then the move will have been made.
    /// First bool of return: if move is legal
    /// Second bool of return: if move is made
    pub fn test_legality_and_maybe_make_move(&mut self, m: Move, rollback: &mut MoveRollback) -> (bool, bool) {
        let mut board_copy = None;
        if ENABLE_UNMAKE_MOVE_TEST {
            board_copy = Some(self.clone());
        }

        let mut result;
        let flags = m.flags();

        if flags == MOVE_KING_CASTLE || flags == MOVE_QUEEN_CASTLE {
            // Check the king isn't in check to begin with
            self.white_to_move = !self.white_to_move;
            result = !self.can_capture_opponent_king(false);
            self.white_to_move = !self.white_to_move;

            if !result {
                return (result, false);
            }

            let direction_sign = if flags == MOVE_KING_CASTLE { 1 } else { -1 };
            let from = m.from();
            let intermediate_index = from.checked_add_signed(direction_sign).unwrap();
            let intermediate_move = Move::new(from as u8, intermediate_index as u8, 0);

            self.make_move(&intermediate_move, rollback);
            result = !self.can_capture_opponent_king(true);
            self.unmake_move(&intermediate_move, rollback);

            if ENABLE_UNMAKE_MOVE_TEST && board_copy.as_ref().unwrap() != self {
                error!("unmake move did not properly undo move {:?}", intermediate_move);
                assert_eq!(board_copy.as_ref().unwrap(), self);
            }

            if !result {
                return (result, false);
            }
        }

        self.make_move(&m, rollback);
        result = !self.can_capture_opponent_king(true);

        (result, true)
    }

    pub fn can_capture_opponent_king(&self, is_legality_test_after_move: bool) -> bool {
        let mut king_pos_opt = None;
        if self.white_to_move {
            if self.piece_counts[1][PIECE_KING as usize] != 0 {
                let opponent_king_piece = PIECE_KING | COLOR_BLACK;
                for i in (0..64).rev() {
                    if self.get_piece_64(i) == opponent_king_piece {
                        king_pos_opt = Some(i);
                        break;
                    }
                }
            }
        } else {
            if self.piece_counts[0][PIECE_KING as usize] != 0 {
                let opponent_king_piece = PIECE_KING;
                for i in 0..64 {
                    if self.get_piece_64(i) == opponent_king_piece {
                        king_pos_opt = Some(i);
                        break;
                    }
                }
            }
        }

        if king_pos_opt.is_none() {
            if is_legality_test_after_move {
                // The king is gone, checkmate already happened
                return true;
            } else {
                panic!("Could not find opponent king in can_capture_opponent_king")
            }
        }
        // Translate from 8x8 to 10x12 board index too
        let king_pos = BOARD_SQUARE_INDEX_TRANSLATION_64[king_pos_opt.unwrap()];

        let color_flag = if self.white_to_move { 0 } else { COLOR_BLACK };

        let queen = PIECE_QUEEN | color_flag;
        let rook = PIECE_ROOK | color_flag;
        let king = PIECE_KING | color_flag;
        for offset in OFFSET[PIECE_ROOK as usize - 2] {
            let mut current_pos = king_pos;
            let mut first = true;
            loop {
                current_pos = current_pos.checked_add_signed(offset).unwrap();
                let piece = self.get_piece(current_pos as usize);
                if piece != PIECE_NONE {
                    if piece == queen || piece == rook || (first && piece == king) {
                        return true;
                    }
                    break;
                }
                first = false;
            }
        }

        let bishop = PIECE_BISHOP | color_flag;
        for offset in OFFSET[PIECE_BISHOP as usize - 2] {
            let mut current_pos = king_pos;
            let mut first = true;
            loop {
                current_pos = current_pos.checked_add_signed(offset).unwrap();
                let piece = self.get_piece(current_pos as usize);
                if piece != PIECE_NONE {
                    if piece == queen || piece == bishop || (first && piece == king) {
                        return true;
                    }
                    break;
                }
                first = false;
            }
        }

        let knight = PIECE_KNIGHT | color_flag;
        for offset in OFFSET[PIECE_KNIGHT as usize - 2] {
            let target_pos = king_pos.checked_add_signed(offset).unwrap() as usize;
            let piece = self.get_piece(target_pos);
            if piece == knight {
                return true;
            }
        }

        let pawn = PIECE_PAWN | color_flag;
        let direction_sign = if self.white_to_move { -1 } else { 1 };
        for offset in [9, 11] {
            let target_pos = king_pos.checked_add_signed(offset * direction_sign).unwrap() as usize;
            let piece = self.get_piece(target_pos);
            if piece == pawn {
                return true;
            }
        }

        false
    }

    #[inline]
    fn do_make_unmake_move_test(&mut self, moves: &Vec<ScoredMove>) {
        if ENABLE_UNMAKE_MOVE_TEST {
            let board_copy = self.clone();
            let mut rollback = MoveRollback::default();

            for m in moves {
                self.make_move(&m.m, &mut rollback);
                self.unmake_move(&m.m, &mut rollback);

                if board_copy != *self {
                    error!("unmake move did not properly undo move {:?}", m.m);

                    if board_copy.hash != self.hash {
                        for (i, v) in HASH_VALUES.iter().enumerate() {
                            if board_copy.hash ^ v == self.hash {
                                debug!("make/unmake differs by value {i} of HASH_VALUES in hash");
                            }
                        }
                    }

                    assert_eq!(board_copy, *self);
                }
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct PerftStats {
    pub nodes: u64,
    pub captures: u64,
    pub eps: u64,
    pub castles: u64,
    pub promotions: u64,
    pub checks: u64,
    pub checkmates: u64,
}

// Code referenced from https://www.chessprogramming.org/Perft
pub fn perft(depth: u8, board: &mut Board, rollback: &mut MoveRollback, stats: &mut PerftStats) {
    if depth == 0 {
        // slow as all heck
        if ENABLE_PERFT_STATS
            && ENABLE_PERFT_STATS_CHECKMATES
            && board.generate_legal_moves_without_history().is_empty()
        {
            stats.checkmates += 1;
        }

        stats.nodes += 1;
        return;
    }

    let moves = board.generate_pseudo_legal_moves_without_history();
    for r#move in &moves {
        let (legal, move_made) = board.test_legality_and_maybe_make_move(r#move.m, rollback);
        if !legal {
            if move_made {
                board.unmake_move(&r#move.m, rollback);
            }

            continue;
        }

        if ENABLE_PERFT_STATS && depth == 1 {
            check_perft_stats(&r#move.m, board, stats);
        }

        perft(depth - 1, board, rollback, stats);
        board.unmake_move(&r#move.m, rollback);
    }
}

fn check_perft_stats(r#move: &Move, board: &mut Board, stats: &mut PerftStats) {
    let flags = r#move.data >> 12;
    if flags & MOVE_FLAG_CAPTURE != 0 {
        stats.captures += 1;

        if flags == MOVE_EP_CAPTURE {
            stats.eps += 1;
        }
    } else if flags == MOVE_KING_CASTLE || flags == MOVE_QUEEN_CASTLE {
        stats.castles += 1;
    }

    if flags & MOVE_FLAG_PROMOTION != 0 {
        stats.promotions += 1;
    }

    board.white_to_move = !board.white_to_move;
    if ENABLE_PERFT_STATS_CHECKS && board.can_capture_opponent_king(false) {
        stats.checks += 1;
    }
    board.white_to_move = !board.white_to_move;
}

pub struct ScoredMove {
    pub m: Move,
    pub score: i16,
}

impl ScoredMove {
    pub fn new(from_square_index: u8, to_square_index: u8, flags: u16, score: i16) -> Self {
        Self {
            m: Move::new(from_square_index, to_square_index, flags),
            score,
        }
    }
}
