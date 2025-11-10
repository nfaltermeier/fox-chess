use log::error;
use tinyvec::{TinyVec, tiny_vec};

use crate::{
    bitboard::{
        BIT_SQUARES, RANK_1, RANK_3, RANK_6, RANK_8, SQUARES_BETWEEN, bitscan_forward_and_reset, lookup_king_attack,
        lookup_knight_attack, lookup_pawn_attack, north_east_one, north_one, north_west_one, south_east_one, south_one,
        south_west_one,
    },
    board::{
        Board, CASTLE_BLACK_KING_FLAG, CASTLE_BLACK_QUEEN_FLAG, CASTLE_WHITE_KING_FLAG, CASTLE_WHITE_QUEEN_FLAG,
        HASH_VALUES, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_QUEEN,
        PIECE_ROOK,
    },
    evaluate::CENTIPAWN_VALUES,
    magic_bitboard::{lookup_bishop_attack, lookup_rook_attack},
    moves::{
        MOVE_DOUBLE_PAWN, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_KING_CASTLE, MOVE_PROMO_BISHOP, MOVE_PROMO_KNIGHT,
        MOVE_PROMO_QUEEN, MOVE_PROMO_ROOK, MOVE_QUEEN_CASTLE, Move, MoveRollback,
    },
    search::{DEFAULT_HISTORY_TABLE, HistoryTable},
};

pub const ENABLE_UNMAKE_MOVE_TEST: bool = false;

/// Has value of target - self added so typical range is +-800. I guess kings capturing have the highest value.
/// Capturing promotions also have value of piece to become added so their additional range is +300 to +1800
const MOVE_SCORE_CAPTURE: i16 = 2000;
pub const MOVE_SCORE_KILLER_1: i16 = 1999;
pub const MOVE_SCORE_KILLER_2: i16 = 1998;
/// Has value of piece is becomes added so really the range is +300 to +900
const MOVE_SCORE_PROMOTION: i16 = 1000;
const MOVE_SCORE_KING_CASTLE: i16 = 999;
const MOVE_SCORE_QUEEN_CASTLE: i16 = 998;
/// No idea what a good value is; only applied to quiet moves. Can also go down to negative this value.
pub const MOVE_SCORE_HISTORY_MAX: i32 = 1400;
pub const MOVE_SCORE_CONST_HISTORY_MAX: i32 = 800;
const MOVE_SCORE_QUIET: i16 = 0;

const MOVE_SCORE_CAPTURE_ATTACKER_DIVISOR: i16 = 10;

pub const MOVE_ARRAY_SIZE: usize = 64;

impl Board {
    #[inline]
    pub fn generate_pseudo_legal_moves_with_history(
        &mut self,
        history_table: &HistoryTable,
    ) -> TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]> {
        let moves = self.generate_moves_pseudo_legal::<true, false>(history_table);

        self.do_make_unmake_move_test(&moves);

        moves
    }

    #[inline]
    pub fn generate_legal_moves_without_history(&mut self) -> TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]> {
        self.generate_legal_moves::<false, false>(&DEFAULT_HISTORY_TABLE)
    }

    #[inline]
    pub fn generate_pseudo_legal_moves_without_history(&mut self) -> TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]> {
        let moves = self.generate_moves_pseudo_legal::<false, false>(&DEFAULT_HISTORY_TABLE);

        self.do_make_unmake_move_test(&moves);

        moves
    }

    #[inline]
    pub fn generate_pseudo_legal_capture_moves(&mut self) -> TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]> {
        let moves = self.generate_moves_pseudo_legal::<false, true>(&DEFAULT_HISTORY_TABLE);

        self.do_make_unmake_move_test(&moves);

        moves
    }

    /// if make and unmake move work properly then at the end board should be back to it's original state
    pub fn generate_legal_moves<const USE_HISTORY: bool, const ONLY_CAPTURES: bool>(
        &mut self,
        history_table: &HistoryTable,
    ) -> TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]> {
        let moves = self.generate_moves_pseudo_legal::<USE_HISTORY, ONLY_CAPTURES>(history_table);
        self.filter_to_legal_moves(moves)
    }

    fn filter_to_legal_moves(
        &mut self,
        mut moves: TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]>,
    ) -> TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]> {
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
                                error!("make/unmake differs by value {i} of HASH_VALUES in hash");
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
    ) -> TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]> {
        let mut result = tiny_vec!();
        let side = if self.white_to_move { 0 } else { 1 };
        let other_side = if self.white_to_move { 1 } else { 0 };

        // Pawn move generation TODO: setwise pawn attacks
        let mut pieces = self.piece_bitboards[side][PIECE_PAWN as usize];
        while pieces != 0 {
            let from = bitscan_forward_and_reset(&mut pieces) as u8;
            let promo =
                (self.white_to_move && (48..=55).contains(&from)) || (!self.white_to_move && (8..=15).contains(&from));

            let mut attacks = lookup_pawn_attack(from, self.white_to_move) & self.side_occupancy[other_side];
            while attacks != 0 {
                let to = bitscan_forward_and_reset(&mut attacks) as u8;
                let target_piece = self.get_piece_64(to as usize);

                if !promo {
                    result.push(ScoredMove {
                        m: Move::new(from, to, MOVE_FLAG_CAPTURE),
                        score: MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES[(target_piece & PIECE_MASK) as usize]
                            - CENTIPAWN_VALUES[PIECE_PAWN as usize] / MOVE_SCORE_CAPTURE_ATTACKER_DIVISOR,
                    });
                } else {
                    let score = MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES[(target_piece & PIECE_MASK) as usize]
                        - CENTIPAWN_VALUES[PIECE_PAWN as usize] / MOVE_SCORE_CAPTURE_ATTACKER_DIVISOR;
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
                    if (!self.white_to_move && (48..=55).contains(&from))
                        || (self.white_to_move && (8..=15).contains(&from))
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
                                        history_table[side][PIECE_PAWN as usize - 1][doublemove_to as usize]
                                    } else {
                                        0
                                    },
                            ));
                        }
                    }
                }
            }
        }

        // En passant
        if let Some(ep_target_64) = self.en_passant_target_square_index {
            let potential_takers = lookup_pawn_attack(ep_target_64, !self.white_to_move);
            let mut takers = potential_takers & self.piece_bitboards[side][PIECE_PAWN as usize];
            while takers != 0 {
                let from = bitscan_forward_and_reset(&mut takers) as u8;

                result.push(ScoredMove {
                    m: Move::new(from, ep_target_64, MOVE_EP_CAPTURE),
                    score: MOVE_SCORE_CAPTURE,
                });
            }
        }

        self.add_moves::<USE_HISTORY, ONLY_CAPTURES, _>(
            PIECE_KNIGHT,
            side,
            other_side,
            &mut result,
            history_table,
            lookup_knight_attack,
        );
        self.add_moves::<USE_HISTORY, ONLY_CAPTURES, _>(
            PIECE_BISHOP,
            side,
            other_side,
            &mut result,
            history_table,
            |sq| lookup_bishop_attack(sq, self.occupancy),
        );
        self.add_moves::<USE_HISTORY, ONLY_CAPTURES, _>(
            PIECE_ROOK,
            side,
            other_side,
            &mut result,
            history_table,
            |sq| lookup_rook_attack(sq, self.occupancy),
        );
        self.add_moves::<USE_HISTORY, ONLY_CAPTURES, _>(
            PIECE_QUEEN,
            side,
            other_side,
            &mut result,
            history_table,
            |sq| lookup_rook_attack(sq, self.occupancy) | lookup_bishop_attack(sq, self.occupancy),
        );
        self.add_moves::<USE_HISTORY, ONLY_CAPTURES, _>(
            PIECE_KING,
            side,
            other_side,
            &mut result,
            history_table,
            lookup_king_attack,
        );

        // Castling
        if !ONLY_CAPTURES && self.castling_rights != 0 {
            if self.white_to_move {
                if self.castling_rights & CASTLE_WHITE_QUEEN_FLAG != 0
                    && (self.occupancy & 0x1f) == 0x11
                    && (self.piece_bitboards[0][PIECE_ROOK as usize] & 0x01) != 0
                {
                    result.push(ScoredMove::new(4, 2, MOVE_QUEEN_CASTLE, MOVE_SCORE_QUEEN_CASTLE));
                }

                if self.castling_rights & CASTLE_WHITE_KING_FLAG != 0
                    && (self.occupancy & 0xf0) == 0x90
                    && (self.piece_bitboards[0][PIECE_ROOK as usize] & 0x80) != 0
                {
                    result.push(ScoredMove::new(4, 6, MOVE_KING_CASTLE, MOVE_SCORE_KING_CASTLE));
                }
            } else {
                if self.castling_rights & CASTLE_BLACK_QUEEN_FLAG != 0
                    && (self.occupancy & 0x1f00000000000000) == 0x1100000000000000
                    && (self.piece_bitboards[1][PIECE_ROOK as usize] & 0x100000000000000) != 0
                {
                    result.push(ScoredMove::new(60, 58, MOVE_QUEEN_CASTLE, MOVE_SCORE_QUEEN_CASTLE));
                }

                if self.castling_rights & CASTLE_BLACK_KING_FLAG != 0
                    && (self.occupancy & 0xf000000000000000) == 0x9000000000000000
                    && (self.piece_bitboards[1][PIECE_ROOK as usize] & 0x8000000000000000) != 0
                {
                    result.push(ScoredMove::new(60, 62, MOVE_KING_CASTLE, MOVE_SCORE_KING_CASTLE));
                }
            }
        }

        result
    }

    fn add_moves<const USE_HISTORY: bool, const ONLY_CAPTURES: bool, F: Fn(u8) -> u64>(
        &self,
        piece_type: u8,
        side: usize,
        other_side: usize,
        result: &mut TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]>,
        history_table: &HistoryTable,
        get_attacks: F,
    ) {
        let mut pieces = self.piece_bitboards[side][piece_type as usize];

        while pieces != 0 {
            let from = bitscan_forward_and_reset(&mut pieces) as u8;

            let mut attacks = get_attacks(from) & !self.side_occupancy[side];

            if ONLY_CAPTURES {
                attacks &= self.side_occupancy[other_side];
            }

            while attacks != 0 {
                let to = bitscan_forward_and_reset(&mut attacks) as u8;
                let target_piece = self.get_piece_64(to as usize);

                if target_piece == PIECE_NONE {
                    result.push(ScoredMove::new(
                        from,
                        to,
                        0,
                        MOVE_SCORE_QUIET
                            + if USE_HISTORY {
                                history_table[side][piece_type as usize - 1][to as usize]
                            } else {
                                0
                            },
                    ));
                } else {
                    result.push(ScoredMove {
                        m: Move::new(from, to, MOVE_FLAG_CAPTURE),
                        score: MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES[(target_piece & PIECE_MASK) as usize]
                            - CENTIPAWN_VALUES[piece_type as usize] / MOVE_SCORE_CAPTURE_ATTACKER_DIVISOR,
                    });
                }
            }
        }
    }

    /// Assumes the king of the side to move is in check
    pub fn generate_pseudo_legal_check_evasions(
        &self,
        history_table: &HistoryTable,
    ) -> TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]> {
        let (self_side, other_side) = if self.white_to_move { (0, 1) } else { (1, 0) };
        let mut result = tiny_vec!();

        let king_pos = self.piece_bitboards[self_side][PIECE_KING as usize].trailing_zeros() as u8;
        if king_pos == 64 {
            panic!("Could not find king in generate_pseudo_legal_check_evasions")
        }

        let double_check;
        let move_to_mask;
        let pawns =
            lookup_pawn_attack(king_pos, self.white_to_move) & self.piece_bitboards[other_side][PIECE_PAWN as usize];
        if pawns != 0 {
            // I'm pretty sure you can't give double check with a pawn
            double_check = false;
            move_to_mask = pawns;
        } else {
            let mut checks = 0;
            let knights = lookup_knight_attack(king_pos) & self.piece_bitboards[other_side][PIECE_KNIGHT as usize];
            if knights != 0 {
                checks += 1;
            }

            let rooks = lookup_rook_attack(king_pos, self.occupancy)
                & (self.piece_bitboards[other_side][PIECE_ROOK as usize]
                    | self.piece_bitboards[other_side][PIECE_QUEEN as usize]);
            if rooks != 0 {
                checks += 1;
            }

            if checks == 2 {
                double_check = true;
                move_to_mask = 0;
            } else {
                let bishops = lookup_bishop_attack(king_pos, self.occupancy)
                    & (self.piece_bitboards[other_side][PIECE_BISHOP as usize]
                        | self.piece_bitboards[other_side][PIECE_QUEEN as usize]);
                if bishops != 0 {
                    checks += 1;
                }

                if checks == 2 {
                    double_check = true;
                    move_to_mask = 0;
                } else if checks == 0 {
                    panic!("Could not find a checking piece in generate_pseudo_legal_check_evasions")
                } else {
                    double_check = false;

                    if knights != 0 {
                        move_to_mask = knights;
                    } else {
                        let piece = if rooks != 0 { rooks } else { bishops };

                        let sq = piece.trailing_zeros();
                        move_to_mask = SQUARES_BETWEEN[king_pos as usize][sq as usize] | piece;
                    }
                }
            }
        }

        self.add_moves::<true, false, _>(PIECE_KING, self_side, other_side, &mut result, history_table, |sq| {
            lookup_king_attack(sq)
        });

        if !double_check {
            self.add_moves::<true, false, _>(PIECE_KNIGHT, self_side, other_side, &mut result, history_table, |sq| {
                lookup_knight_attack(sq) & move_to_mask
            });
            self.add_moves::<true, false, _>(PIECE_BISHOP, self_side, other_side, &mut result, history_table, |sq| {
                lookup_bishop_attack(sq, self.occupancy) & move_to_mask
            });
            self.add_moves::<true, false, _>(PIECE_ROOK, self_side, other_side, &mut result, history_table, |sq| {
                lookup_rook_attack(sq, self.occupancy) & move_to_mask
            });
            self.add_moves::<true, false, _>(PIECE_QUEEN, self_side, other_side, &mut result, history_table, |sq| {
                (lookup_rook_attack(sq, self.occupancy) | lookup_bishop_attack(sq, self.occupancy)) & move_to_mask
            });

            if self.piece_bitboards[self_side][PIECE_PAWN as usize] != 0 {
                let pawns = self.piece_bitboards[self_side][PIECE_PAWN as usize];

                // Pawn captures
                for round in 0..2 {
                    let mut moves;
                    let offset;
                    if self.white_to_move {
                        match round {
                            0 => {
                                moves = north_east_one(pawns);
                                offset = -9;
                            }
                            1 => {
                                moves = north_west_one(pawns);
                                offset = -7;
                            }
                            _ => panic!(),
                        }
                    } else {
                        match round {
                            0 => {
                                moves = south_east_one(pawns);
                                offset = 7;
                            }
                            1 => {
                                moves = south_west_one(pawns);
                                offset = 9;
                            }
                            _ => panic!(),
                        }
                    }

                    moves &= self.side_occupancy[other_side];
                    moves &= move_to_mask;
                    self.add_pawn_moves::<false, true, false, false>(
                        moves,
                        offset,
                        &mut result,
                        self_side,
                        history_table,
                    );
                    self.add_pawn_moves::<false, true, true, false>(
                        moves,
                        offset,
                        &mut result,
                        self_side,
                        history_table,
                    );
                }

                let mut moves;
                let mut offset;
                if self.white_to_move {
                    moves = north_one(pawns);
                    offset = -8;
                } else {
                    moves = south_one(pawns);
                    offset = 8;
                }

                moves &= !self.occupancy;
                let masked_moves = moves & move_to_mask;
                self.add_pawn_moves::<true, false, false, false>(
                    masked_moves,
                    offset,
                    &mut result,
                    self_side,
                    history_table,
                );
                self.add_pawn_moves::<true, false, true, false>(
                    masked_moves,
                    offset,
                    &mut result,
                    self_side,
                    history_table,
                );

                if self.white_to_move {
                    // The pawns have already been shifted forward once so mask to only ranks 3 and 6 instead of 2 and 7
                    moves &= RANK_3;
                    moves = north_one(moves);
                    offset = -16;
                } else {
                    moves &= RANK_6;
                    moves = south_one(moves);
                    offset = 16;
                }

                moves &= !self.occupancy;
                moves &= move_to_mask;
                self.add_pawn_moves::<true, false, false, true>(moves, offset, &mut result, self_side, history_table);

                // En passant
                if let Some(ep_target_64) = self.en_passant_target_square_index {
                    if move_to_mask
                        & BIT_SQUARES[ep_target_64
                            .checked_add_signed(if self.white_to_move { -8 } else { 8 })
                            .unwrap() as usize]
                        != 0
                    {
                        let potential_takers = lookup_pawn_attack(ep_target_64, !self.white_to_move);
                        let mut takers = potential_takers & self.piece_bitboards[self_side][PIECE_PAWN as usize];
                        while takers != 0 {
                            let from = bitscan_forward_and_reset(&mut takers) as u8;

                            result.push(ScoredMove {
                                m: Move::new(from, ep_target_64, MOVE_EP_CAPTURE),
                                score: MOVE_SCORE_CAPTURE,
                            });
                        }
                    }
                }
            }
        }

        result
    }

    fn add_pawn_moves<const USE_HISTORY: bool, const CAPTURES: bool, const PROMOS: bool, const DOUBLE_PUSH: bool>(
        &self,
        mut to_squares: u64,
        offset: i8,
        result: &mut TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]>,
        side: usize,
        history_table: &HistoryTable,
    ) {
        if PROMOS {
            to_squares &= RANK_1 | RANK_8;
        } else {
            to_squares &= !(RANK_1 | RANK_8);
        }

        while to_squares != 0 {
            let to = bitscan_forward_and_reset(&mut to_squares) as u8;
            let from = to.checked_add_signed(offset).unwrap();

            let score = if !CAPTURES {
                MOVE_SCORE_QUIET
                    + if USE_HISTORY {
                        history_table[side][PIECE_PAWN as usize - 1][to as usize]
                    } else {
                        0
                    }
            } else {
                let target_piece = self.get_piece_64(to as usize);
                MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES[(target_piece & PIECE_MASK) as usize]
                    - CENTIPAWN_VALUES[PIECE_PAWN as usize] / MOVE_SCORE_CAPTURE_ATTACKER_DIVISOR
            };

            if !PROMOS {
                result.push(ScoredMove {
                    m: Move::new(
                        from,
                        to,
                        if CAPTURES { MOVE_FLAG_CAPTURE } else { 0 } | if DOUBLE_PUSH { MOVE_DOUBLE_PAWN } else { 0 },
                    ),
                    score,
                });
            } else {
                result.push(ScoredMove::new(
                    from,
                    to,
                    MOVE_PROMO_QUEEN | if CAPTURES { MOVE_FLAG_CAPTURE } else { 0 },
                    score + CENTIPAWN_VALUES[PIECE_QUEEN as usize],
                ));
                result.push(ScoredMove::new(
                    from,
                    to,
                    MOVE_PROMO_ROOK | if CAPTURES { MOVE_FLAG_CAPTURE } else { 0 },
                    score + CENTIPAWN_VALUES[PIECE_ROOK as usize],
                ));
                result.push(ScoredMove::new(
                    from,
                    to,
                    MOVE_PROMO_BISHOP | if CAPTURES { MOVE_FLAG_CAPTURE } else { 0 },
                    score + CENTIPAWN_VALUES[PIECE_BISHOP as usize],
                ));
                result.push(ScoredMove::new(
                    from,
                    to,
                    MOVE_PROMO_KNIGHT | if CAPTURES { MOVE_FLAG_CAPTURE } else { 0 },
                    score + CENTIPAWN_VALUES[PIECE_KNIGHT as usize],
                ));
            }
        }
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
            if self.is_in_check(false) {
                return (false, false);
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

    #[inline]
    pub fn is_in_check(&mut self, is_legality_test_after_move: bool) -> bool {
        self.white_to_move = !self.white_to_move;
        let result = self.can_capture_opponent_king(is_legality_test_after_move);
        self.white_to_move = !self.white_to_move;

        result
    }

    pub fn can_capture_opponent_king(&self, is_legality_test_after_move: bool) -> bool {
        let side = if self.white_to_move { 0 } else { 1 };
        let other_side = if self.white_to_move { 1 } else { 0 };

        let king_pos = self.piece_bitboards[other_side as usize][PIECE_KING as usize].trailing_zeros() as u8;
        if king_pos == 64 {
            if is_legality_test_after_move {
                // The king is gone, checkmate already happened
                return true;
            } else {
                panic!("Could not find opponent king in can_capture_opponent_king")
            }
        }

        let danger_rooks = self.piece_bitboards[side as usize][PIECE_ROOK as usize]
            | self.piece_bitboards[side as usize][PIECE_QUEEN as usize];
        if danger_rooks != 0 && lookup_rook_attack(king_pos, self.occupancy) & danger_rooks != 0 {
            return true;
        }

        let danger_bishops = self.piece_bitboards[side as usize][PIECE_BISHOP as usize]
            | self.piece_bitboards[side as usize][PIECE_QUEEN as usize];
        if danger_bishops != 0 && lookup_bishop_attack(king_pos, self.occupancy) & danger_bishops != 0 {
            return true;
        }

        let knights = self.piece_bitboards[side as usize][PIECE_KNIGHT as usize];
        if knights != 0 && lookup_knight_attack(king_pos) & knights != 0 {
            return true;
        }

        let pawns = self.piece_bitboards[side as usize][PIECE_PAWN as usize];
        if pawns != 0 && lookup_pawn_attack(king_pos, !self.white_to_move) & pawns != 0 {
            return true;
        }

        if lookup_king_attack(king_pos) & self.piece_bitboards[side as usize][PIECE_KING as usize] != 0 {
            return true;
        }

        false
    }

    #[inline]
    fn do_make_unmake_move_test(&mut self, moves: &TinyVec<[ScoredMove; MOVE_ARRAY_SIZE]>) {
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
                                error!("make/unmake differs by value {i} of HASH_VALUES in hash");
                            }
                        }
                    }

                    assert_eq!(board_copy, *self);
                }
            }
        }
    }
}

#[derive(Default)]
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

#[cfg(test)]
mod check_evasion_tests {
    use super::*;
    use crate::initialize_magic_bitboards;

    macro_rules! check_evasion_tests_legal_moves {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;

                    initialize_magic_bitboards();

                    let mut board = Board::from_fen(input).unwrap();
                    let evasion_generator_moves = board.generate_pseudo_legal_check_evasions(&DEFAULT_HISTORY_TABLE);
                    let legal_evasion_generator_moves = board.filter_to_legal_moves(evasion_generator_moves);

                    assert_eq!(expected, legal_evasion_generator_moves.len());

                    assert_eq!(expected, board.generate_legal_moves_without_history().len());
                }
            )*
        }
    }

    check_evasion_tests_legal_moves! {
        guarded_pawn: ("k7/8/2p5/3p4/4K3/8/8/8 w - - 0 1", 7),
        block_or_take_rook: ("k7/8/4B3/8/r3K3/3P4/Q1N5/8 w - - 0 1", 11),
        double_check: ("k7/7b/4B3/8/r3K3/3P4/Q1N5/8 w - - 0 1", 4),
        pawns_use_correct_direction_white: ("k7/8/8/3p1p2/r3K3/4P3/8/8 w - - 0 1", 5),
        pawns_use_correct_direction_black: ("K7/8/8/3P1P2/R3k3/4p3/8/8 b - - 0 1", 5),
        en_passant_white: ("k7/8/2p5/3pP3/4K3/8/8/8 w - d6 0 1", 7),
        en_passant_black: ("K7/8/8/4k3/3Pp3/2P5/8/8 b - d3 0 1", 7),
        promotion_white: ("K5r1/5P2/8/4k3/8/8/8/8 w - - 0 1", 10),
        promotion_black: ("8/8/8/8/4K3/8/5p2/k5R1 b - - 0 1", 10),
        pawn_double_move_white: ("8/8/3k4/8/3K3r/8/5P2/8 w - - 0 1", 4),
        pawn_double_move_black: ("8/5p2/8/3k3R/8/3K4/8/8 b - - 0 1", 4),
        pawn_block_or_take_white: ("k2r4/8/8/7r/1r2K3/2P4r/8/5r2 w - - 0 1", 2),
        pawn_block_or_take_black: ("K2R4/8/2p4R/1R2k3/7R/8/8/5R2 b - - 0 1", 2),
    }
}
