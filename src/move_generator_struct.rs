use arrayvec::ArrayVec;

use crate::{
    bitboard::{
        RANK_1, RANK_3, RANK_6, RANK_8, bitscan_forward_and_reset, lookup_king_attack, lookup_knight_attack,
        lookup_pawn_attack, north_east_one, north_one, north_west_one, south_east_one, south_one, south_west_one,
    },
    board::{
        Board, CASTLE_BLACK_KING_FLAG, CASTLE_BLACK_QUEEN_FLAG, CASTLE_WHITE_KING_FLAG, CASTLE_WHITE_QUEEN_FLAG,
        PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_MASK, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK,
    },
    eval_values::CENTIPAWN_VALUES_MIDGAME,
    magic_bitboard::{lookup_bishop_attack, lookup_rook_attack},
    move_generator::{
        MOVE_ARRAY_SIZE, MOVE_SCORE_CAPTURE, MOVE_SCORE_CAPTURE_ATTACKER_DIVISOR, MOVE_SCORE_CONST_HISTORY_MAX,
        MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_KILLER_1, MOVE_SCORE_KILLER_2, MOVE_SCORE_KING_CASTLE,
        MOVE_SCORE_QUEEN_CASTLE, MOVE_SCORE_QUIET, ScoredMove,
    },
    moves::{
        MOVE_DOUBLE_PAWN, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_CAPTURE_FULL, MOVE_KING_CASTLE,
        MOVE_PROMO_BISHOP, MOVE_PROMO_KNIGHT, MOVE_PROMO_QUEEN, MOVE_PROMO_ROOK, MOVE_QUEEN_CASTLE, Move,
    },
    search::{ContinuationHistoryTable, DEFAULT_HISTORY_TABLE, EMPTY_MOVE, HistoryTable},
};

pub struct MoveGenerator {
    move_buf: ArrayVec<ScoredMove, MOVE_ARRAY_SIZE>,
    moves_selected: usize,
    tt_move: Move,
    state: MoveGeneratorState,
    pending_moves: ArrayVec<PendingMoves, 20>,
    tt_move_used: bool,
}

struct PendingMoves {
    pub targets: u64,
    pub from: u8,
}

pub enum GetMoveResult {
    Move(ScoredMove),
    GenerateMoves,
    NoMoves,
}

#[derive(PartialEq, Eq)]
enum MoveGeneratorState {
    AllGenerated,
    GeneratePending,
    GeneratePseudolegal,
    GenerateCheckEvasions,
}

impl MoveGenerator {
    pub fn new() -> Self {
        Self {
            move_buf: ArrayVec::new(),
            moves_selected: 0,
            tt_move: EMPTY_MOVE,
            state: MoveGeneratorState::AllGenerated,
            pending_moves: ArrayVec::new(),
            tt_move_used: true,
        }
    }

    pub fn set_tt_move(&mut self, m: Move) {
        self.move_buf.clear();
        self.move_buf.push(ScoredMove { m, score: i16::MAX });
        self.tt_move = m;
        self.tt_move_used = false;
    }

    pub fn get_next_move(&mut self) -> GetMoveResult {
        if self.moves_selected >= self.move_buf.len() {
            if self.state == MoveGeneratorState::AllGenerated {
                return GetMoveResult::NoMoves;
            } else {
                return GetMoveResult::GenerateMoves;
            }
        }

        if !self.tt_move_used && self.tt_move != EMPTY_MOVE {
            self.tt_move_used = true;
            return GetMoveResult::Move(ScoredMove {
                m: self.tt_move,
                score: 1,
            });
        }

        select_next_move(&mut self.move_buf, self.moves_selected);
        let m = self.move_buf[self.moves_selected];

        if m.m == self.tt_move {
            self.moves_selected += 1;
            return self.get_next_move();
        }

        if m.score > (MOVE_SCORE_HISTORY_MAX + MOVE_SCORE_CONST_HISTORY_MAX) as i16
            || self.state == MoveGeneratorState::AllGenerated
        {
            self.moves_selected += 1;
            GetMoveResult::Move(m)
        } else {
            GetMoveResult::GenerateMoves
        }
    }

    /// For perft, does not sort moves and expects tt move to be unset
    pub fn get_next_move_unordered(&mut self) -> GetMoveResult {
        if self.moves_selected >= self.move_buf.len() {
            if self.state == MoveGeneratorState::AllGenerated {
                return GetMoveResult::NoMoves;
            } else {
                return GetMoveResult::GenerateMoves;
            }
        }

        let m = self.move_buf[self.moves_selected];
        self.moves_selected += 1;
        GetMoveResult::Move(m)
    }

    pub fn generate_more_moves(
        &mut self,
        board: &mut Board,
        history_table: Option<&HistoryTable>,
        move_history: Option<&Vec<Move>>,
        continuation_history: Option<&ContinuationHistoryTable>,
        killers: Option<&[Move; 2]>,
    ) {
        match self.state {
            MoveGeneratorState::AllGenerated => {}
            MoveGeneratorState::GeneratePending => self.generate_pending_moves(
                board,
                history_table.unwrap_or(&DEFAULT_HISTORY_TABLE),
                move_history,
                continuation_history,
                killers,
            ),
            MoveGeneratorState::GeneratePseudolegal => self.generate_moves_pseudo_legal_impl(board),
            MoveGeneratorState::GenerateCheckEvasions => {
                self.generate_moves_check_evasion(board, history_table, move_history, continuation_history, killers)
            }
        }
    }

    pub fn generate_moves_pseudo_legal(&mut self, board: &mut Board) {
        // If no tt move
        if self.move_buf.is_empty() {
            self.generate_moves_pseudo_legal_impl(board);
        } else {
            // Moves will be generated after tt move is searched
            self.state = MoveGeneratorState::GeneratePseudolegal;
        }
    }

    pub fn generate_moves_check_evasion(
        &mut self,
        board: &mut Board,
        history_table: Option<&HistoryTable>,
        move_history: Option<&Vec<Move>>,
        continuation_history: Option<&ContinuationHistoryTable>,
        killers: Option<&[Move; 2]>,
    ) {
        // If no tt move
        if self.move_buf.is_empty() || self.tt_move_used {
            board.generate_pseudo_legal_check_evasions(
                history_table.unwrap_or(&DEFAULT_HISTORY_TABLE),
                &mut self.move_buf,
            );
            self.state = MoveGeneratorState::AllGenerated;
            // board.generate_pseudo_legal_check_evasions clears the move buffer
            self.moves_selected = 0;

            extra_quiet_move_scoring(&mut self.move_buf, board, move_history, continuation_history, killers);
        } else {
            // Moves will be generated after tt move is searched
            self.state = MoveGeneratorState::GenerateCheckEvasions;
        }
    }

    fn generate_pending_moves(
        &mut self,
        board: &mut Board,
        history_table: &HistoryTable,
        move_history: Option<&Vec<Move>>,
        continuation_history: Option<&ContinuationHistoryTable>,
        killers: Option<&[Move; 2]>,
    ) {
        let starting_move_buf_len = self.move_buf.len();
        let side = if board.white_to_move { 0 } else { 1 };

        let (mv, offset, double_push_intermediate): (fn(u64) -> u64, i8, u64) = if board.white_to_move {
            (north_one, -8, RANK_3)
        } else {
            (south_one, 8, RANK_6)
        };
        let pawn_advance = mv(board.piece_bitboards[side][PIECE_PAWN as usize]) & !board.occupancy;
        let mut promos = pawn_advance & (RANK_1 | RANK_8);
        let mut non_promos = pawn_advance & !(RANK_1 | RANK_8);
        let mut double_push = mv(non_promos & double_push_intermediate) & !board.occupancy;

        while non_promos != 0 {
            let to = bitscan_forward_and_reset(&mut non_promos) as u8;
            let from = to.checked_add_signed(offset).unwrap();

            self.move_buf.push(ScoredMove {
                m: Move::new(from, to, 0),
                score: MOVE_SCORE_QUIET + history_table[side][PIECE_PAWN as usize - 1][to as usize],
            });
        }

        while promos != 0 {
            let to = bitscan_forward_and_reset(&mut promos) as u8;
            let from = to.checked_add_signed(offset).unwrap();

            let base_score = MOVE_SCORE_QUIET + history_table[side][PIECE_PAWN as usize - 1][to as usize];
            self.move_buf.push(ScoredMove {
                m: Move::new(from, to, MOVE_PROMO_QUEEN),
                score: base_score + CENTIPAWN_VALUES_MIDGAME[PIECE_QUEEN as usize],
            });
            self.move_buf.push(ScoredMove {
                m: Move::new(from, to, MOVE_PROMO_ROOK),
                score: base_score + CENTIPAWN_VALUES_MIDGAME[PIECE_ROOK as usize],
            });
            self.move_buf.push(ScoredMove {
                m: Move::new(from, to, MOVE_PROMO_BISHOP),
                score: base_score + CENTIPAWN_VALUES_MIDGAME[PIECE_BISHOP as usize],
            });
            self.move_buf.push(ScoredMove {
                m: Move::new(from, to, MOVE_PROMO_KNIGHT),
                score: base_score + CENTIPAWN_VALUES_MIDGAME[PIECE_KNIGHT as usize],
            });
        }

        let offset = offset * 2;
        while double_push != 0 {
            let to = bitscan_forward_and_reset(&mut double_push) as u8;
            let from = to.checked_add_signed(offset).unwrap();

            self.move_buf.push(ScoredMove {
                m: Move::new(from, to, MOVE_DOUBLE_PAWN),
                score: MOVE_SCORE_QUIET + history_table[side][PIECE_PAWN as usize - 1][to as usize],
            });
        }

        // generate moves, assuming all quiet moves need to be generated
        for pending in &mut self.pending_moves {
            let piece_type = board.get_piece_64(pending.from as usize) & PIECE_MASK;

            while pending.targets != 0 {
                let to = bitscan_forward_and_reset(&mut pending.targets) as u8;

                self.move_buf.push(ScoredMove::new(
                    pending.from,
                    to,
                    0,
                    MOVE_SCORE_QUIET + history_table[side][piece_type as usize - 1][to as usize],
                ));
            }
        }

        self.state = MoveGeneratorState::AllGenerated;
        let move_buf_len = self.move_buf.len();

        extra_quiet_move_scoring(
            &mut self.move_buf[starting_move_buf_len..move_buf_len],
            board,
            move_history,
            continuation_history,
            killers,
        );
    }

    /// set_tt_move() should be called before this method, if it is going to be called
    fn generate_moves_pseudo_legal_impl(&mut self, board: &mut Board) {
        let side = if board.white_to_move { 0 } else { 1 };
        let other_side = if board.white_to_move { 1 } else { 0 };
        let occ = board.occupancy;

        self.state = MoveGeneratorState::GeneratePending;

        let pawn_data: [(fn(u64) -> u64, i8); 2] = if board.white_to_move {
            [(north_east_one, -9), (north_west_one, -7)]
        } else {
            [(south_east_one, 7), (south_west_one, 9)]
        };

        for (mv, offset) in pawn_data {
            let targets = mv(board.piece_bitboards[side][PIECE_PAWN as usize]) & board.side_occupancy[other_side];
            let mut promos = targets & (RANK_1 | RANK_8);
            let mut non_promos = targets & !(RANK_1 | RANK_8);

            while non_promos != 0 {
                let to = bitscan_forward_and_reset(&mut non_promos) as u8;
                let from = to.checked_add_signed(offset).unwrap();
                let target_piece = board.get_piece_64(to as usize);

                self.move_buf.push(ScoredMove {
                    m: Move::new(from, to, MOVE_FLAG_CAPTURE),
                    score: MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES_MIDGAME[(target_piece & PIECE_MASK) as usize]
                        - CENTIPAWN_VALUES_MIDGAME[PIECE_PAWN as usize] / MOVE_SCORE_CAPTURE_ATTACKER_DIVISOR,
                });
            }

            while promos != 0 {
                let to = bitscan_forward_and_reset(&mut promos) as u8;
                let from = to.checked_add_signed(offset).unwrap();
                let target_piece = board.get_piece_64(to as usize);

                let base_score = MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES_MIDGAME[(target_piece & PIECE_MASK) as usize]
                    - CENTIPAWN_VALUES_MIDGAME[PIECE_PAWN as usize] / MOVE_SCORE_CAPTURE_ATTACKER_DIVISOR;
                self.move_buf.push(ScoredMove {
                    m: Move::new(from, to, MOVE_FLAG_CAPTURE | MOVE_PROMO_QUEEN),
                    score: base_score + CENTIPAWN_VALUES_MIDGAME[PIECE_QUEEN as usize],
                });
                self.move_buf.push(ScoredMove {
                    m: Move::new(from, to, MOVE_FLAG_CAPTURE | MOVE_PROMO_ROOK),
                    score: base_score + CENTIPAWN_VALUES_MIDGAME[PIECE_ROOK as usize],
                });
                self.move_buf.push(ScoredMove {
                    m: Move::new(from, to, MOVE_FLAG_CAPTURE | MOVE_PROMO_BISHOP),
                    score: base_score + CENTIPAWN_VALUES_MIDGAME[PIECE_BISHOP as usize],
                });
                self.move_buf.push(ScoredMove {
                    m: Move::new(from, to, MOVE_FLAG_CAPTURE | MOVE_PROMO_KNIGHT),
                    score: base_score + CENTIPAWN_VALUES_MIDGAME[PIECE_KNIGHT as usize],
                });
            }
        }

        if let Some(ep_target_64) = board.en_passant_target_square_index {
            let potential_takers = lookup_pawn_attack(ep_target_64, !board.white_to_move);
            let mut takers = potential_takers & board.piece_bitboards[side][PIECE_PAWN as usize];
            while takers != 0 {
                let from = bitscan_forward_and_reset(&mut takers) as u8;

                self.move_buf.push(ScoredMove {
                    m: Move::new(from, ep_target_64, MOVE_EP_CAPTURE),
                    score: MOVE_SCORE_CAPTURE,
                });
            }
        }

        self.add_captures_and_delay_quiets::<_>(board, PIECE_KNIGHT, side, other_side, lookup_knight_attack);
        self.add_captures_and_delay_quiets::<_>(board, PIECE_BISHOP, side, other_side, |sq| {
            lookup_bishop_attack(sq, occ)
        });
        self.add_captures_and_delay_quiets::<_>(board, PIECE_ROOK, side, other_side, |sq| lookup_rook_attack(sq, occ));
        self.add_captures_and_delay_quiets::<_>(board, PIECE_QUEEN, side, other_side, |sq| {
            lookup_rook_attack(sq, occ) | lookup_bishop_attack(sq, occ)
        });
        self.add_captures_and_delay_quiets::<_>(board, PIECE_KING, side, other_side, lookup_king_attack);
    }

    fn add_captures_and_delay_quiets<F: Fn(u8) -> u64>(
        &mut self,
        board: &mut Board,
        piece_type: u8,
        side: usize,
        other_side: usize,
        get_attacks: F,
    ) {
        let mut pieces = board.piece_bitboards[side][piece_type as usize];

        while pieces != 0 {
            let from = bitscan_forward_and_reset(&mut pieces) as u8;

            let all_attacks = get_attacks(from) & !board.side_occupancy[side];
            let mut captures = all_attacks & board.side_occupancy[other_side];
            let quiets = all_attacks & !board.side_occupancy[other_side];

            if quiets != 0 {
                self.pending_moves.push(PendingMoves { targets: quiets, from });
            }

            while captures != 0 {
                let to = bitscan_forward_and_reset(&mut captures) as u8;
                let target_piece = board.get_piece_64(to as usize);

                self.move_buf.push(ScoredMove {
                    m: Move::new(from, to, MOVE_FLAG_CAPTURE),
                    score: MOVE_SCORE_CAPTURE + CENTIPAWN_VALUES_MIDGAME[(target_piece & PIECE_MASK) as usize]
                        - CENTIPAWN_VALUES_MIDGAME[piece_type as usize] / MOVE_SCORE_CAPTURE_ATTACKER_DIVISOR,
                });
            }
        }
    }
}

fn apply_continuation_history_to_move_scores(
    move_history: &Vec<Move>,
    board: &Board,
    continuation_history: &ContinuationHistoryTable,
    moves: &mut [ScoredMove],
) {
    if let Some(relevant_cont_hist) = get_relevant_cont_hist(move_history, board, continuation_history) {
        for m in moves {
            if m.m.data & MOVE_FLAG_CAPTURE_FULL != 0 {
                continue;
            }

            let piece_to_move = board.get_piece_64(m.m.from() as usize);
            m.score += relevant_cont_hist[((piece_to_move & PIECE_MASK) - 1) as usize][m.m.to() as usize];
        }
    }
}

#[inline]
fn get_relevant_cont_hist<'a>(
    move_history: &[Move],
    board: &Board,
    continuation_history: &'a ContinuationHistoryTable,
) -> Option<&'a [[i16; 64]; 6]> {
    if let Some(last_move) = move_history.last()
        && *last_move != EMPTY_MOVE
    {
        let last_moved_to = last_move.to() as usize;
        let last_moved_piece = board.get_piece_64(last_moved_to);
        return Some(
            &continuation_history[if board.white_to_move { 0 } else { 1 }]
                [((last_moved_piece & PIECE_MASK) - 1) as usize][last_moved_to],
        );
    }

    None
}

#[inline]
fn select_next_move(moves: &mut ArrayVec<ScoredMove, MOVE_ARRAY_SIZE>, index_to_skip: usize) {
    // Perform one iteration of selection sort every time another move needs to be evaluated
    let mut best_move_score = moves[index_to_skip].score;
    let mut best_move_index = index_to_skip;

    for sort_index in (index_to_skip + 1)..moves.len() {
        if moves[sort_index].score > best_move_score {
            best_move_score = moves[sort_index].score;
            best_move_index = sort_index;
        }
    }

    moves.swap(index_to_skip, best_move_index);
}

fn extra_quiet_move_scoring(
    moves: &mut [ScoredMove],
    board: &Board,
    move_history: Option<&Vec<Move>>,
    continuation_history: Option<&ContinuationHistoryTable>,
    killers: Option<&[Move; 2]>,
) {
    if let Some(move_history) = move_history
        && let Some(continuation_history) = continuation_history
    {
        apply_continuation_history_to_move_scores(move_history, board, continuation_history, moves);
    }

    if let Some(killers) = killers
        && killers[0] != EMPTY_MOVE
    {
        let mut unmatched_killers = if killers[1] != EMPTY_MOVE { 2 } else { 1 };

        for m in moves {
            if m.m == killers[0] {
                m.score = MOVE_SCORE_KILLER_1;

                if unmatched_killers == 1 {
                    break;
                } else {
                    unmatched_killers -= 1;
                }
            } else if m.m == killers[1] {
                m.score = MOVE_SCORE_KILLER_2;

                if unmatched_killers == 1 {
                    break;
                } else {
                    unmatched_killers -= 1;
                }
            }
        }
    }
}
