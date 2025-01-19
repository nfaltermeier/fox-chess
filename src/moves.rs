use log::{debug, error};
use regex::Regex;

use crate::{
    board::{
        file_8x8, get_hash_value, index_8x8_to_pos_str, piece_to_name, rank_8x8, Board, CastlingValue, COLOR_BLACK,
        HASH_VALUES, HASH_VALUES_BLACK_TO_MOVE_IDX, HASH_VALUES_CASTLE_BASE_IDX, HASH_VALUES_EP_FILE_IDX, PIECE_KING,
        PIECE_MASK, PIECE_NONE, PIECE_PAWN, PIECE_ROOK,
    },
    evaluate::GAME_STAGE_VALUES,
    move_generator::{generate_legal_moves, generate_legal_moves_without_history, generate_pseudo_legal_moves_without_history, test_legality_and_maybe_make_move, ScoredMove, ENABLE_UNMAKE_MOVE_TEST},
    search::DEFAULT_HISTORY_TABLE,
    STARTING_FEN,
};

// Assumes flags have been shifted to bits 1-4
pub const MOVE_FLAG_PROMOTION: u16 = 1 << 3;
pub const MOVE_FLAG_CAPTURE: u16 = 1 << 2;

pub const MOVE_DOUBLE_PAWN: u16 = 1;
pub const MOVE_KING_CASTLE: u16 = 2;
pub const MOVE_QUEEN_CASTLE: u16 = 3;
pub const MOVE_EP_CAPTURE: u16 = 5;
pub const FLAGS_PROMO_KNIGHT: u16 = 0;
pub const FLAGS_PROMO_BISHOP: u16 = 1;
pub const FLAGS_PROMO_ROOK: u16 = 2;
pub const FLAGS_PROMO_QUEEN: u16 = 3;
pub const MOVE_PROMO_KNIGHT: u16 = MOVE_FLAG_PROMOTION | FLAGS_PROMO_KNIGHT;
pub const MOVE_PROMO_BISHOP: u16 = MOVE_FLAG_PROMOTION | FLAGS_PROMO_BISHOP;
pub const MOVE_PROMO_ROOK: u16 = MOVE_FLAG_PROMOTION | FLAGS_PROMO_ROOK;
pub const MOVE_PROMO_QUEEN: u16 = MOVE_FLAG_PROMOTION | FLAGS_PROMO_QUEEN;
pub const FLAGS_MASK_PROMO: u16 = 3;

pub const MOVE_FLAG_CAPTURE_FULL: u16 = MOVE_FLAG_CAPTURE << 12;

#[derive(PartialEq, Eq, Copy, Clone)]
pub struct Move {
    // from: 6 bits, to: 6 bits: flags: 4 bits. Using flags format from https://www.chessprogramming.org/Encoding_Moves
    pub data: u16,
}

impl Move {
    pub fn new(from_square_index: u8, to_square_index: u8, flags: u16) -> Move {
        Move {
            data: from_square_index as u16 | ((to_square_index as u16) << 6) | (flags << 12),
        }
    }

    pub fn from(&self) -> u16 {
        self.data & 0x003F
    }

    pub fn to(&self) -> u16 {
        (self.data >> 6) & 0x003F
    }

    pub fn flags(&self) -> u16 {
        self.data >> 12
    }

    pub fn pretty_print(&self, board: Option<&Board>) -> String {
        let flags = self.flags();

        if flags == MOVE_KING_CASTLE {
            return String::from("0-0");
        } else if flags == MOVE_QUEEN_CASTLE {
            return String::from("0-0-0");
        }

        let capture = (flags & MOVE_FLAG_CAPTURE) != 0;
        let capture_char = if capture { 'x' } else { '-' };

        let mut promoted_to = ' ';
        if flags & MOVE_FLAG_PROMOTION != 0 {
            let color_flag = match board {
                Some(b) => {
                    if b.white_to_move {
                        0
                    } else {
                        COLOR_BLACK
                    }
                }
                None => 0,
            };
            let promo_value = (flags as u8) & 3;
            // +2 converts promo code to piece code
            let promo_to_piece = color_flag | (promo_value + 2);

            promoted_to = piece_to_name(promo_to_piece);
        }

        let from = self.from() as u8;
        let to = self.to() as u8;
        let piece_name = match board {
            Some(b) => {
                let piece = b.get_piece_64(from as usize);
                piece_to_name(piece)
            }
            None => '?',
        };

        let from_rank = rank_8x8(from);
        let from_file = file_8x8(from);
        let to_rank = rank_8x8(to);
        let to_file = file_8x8(to);

        format!(
            "{}{}{}{}{}{}{}",
            piece_name,
            (b'a' + from_file) as char,
            from_rank,
            capture_char,
            (b'a' + to_file) as char,
            to_rank,
            promoted_to
        )
    }

    pub fn simple_long_algebraic_notation(&self) -> String {
        let from = self.from() as u8;
        let to = self.to() as u8;
        let flags = self.flags();

        let from_rank = rank_8x8(from);
        let from_file = file_8x8(from);
        let to_rank = rank_8x8(to);
        let to_file = file_8x8(to);

        let result = format!(
            "{}{}{}{}",
            (b'a' + from_file) as char,
            from_rank,
            (b'a' + to_file) as char,
            to_rank,
        );

        if flags & MOVE_FLAG_PROMOTION == 0 {
            result
        } else {
            let promo_value = (flags as u8) & 3;
            // +2 converts promo code to piece code
            let promo_to_piece = COLOR_BLACK | (promo_value + 2);

            format!("{}{}", result, piece_to_name(promo_to_piece))
        }
    }
}

impl std::fmt::Debug for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Move from: {} to: {} flags: {}\nPretty: {}",
            self.from(),
            self.to(),
            self.flags(),
            self.pretty_print(None)
        )
    }
}

#[derive(Debug, Default)]
pub struct MoveRollback {
    // Only added when a piece is actually captured
    pub captured_pieces: Vec<u8>,
    pub ep_index: Vec<Option<u8>>,
    pub castling_rights: Vec<u8>,
    pub halfmove_clocks: Vec<u8>,
}

impl MoveRollback {
    pub fn is_empty(&self) -> bool {
        self.captured_pieces.is_empty()
            && self.ep_index.is_empty()
            && self.castling_rights.is_empty()
            && self.halfmove_clocks.is_empty()
    }
}

impl Board {
    pub fn make_move(&mut self, r#move: &Move, rollback: &mut MoveRollback) {
        let from = r#move.from() as usize;
        let to = r#move.to() as usize;
        let flags = r#move.flags();
        let hash_values = &*HASH_VALUES;

        let capture = (flags & MOVE_FLAG_CAPTURE) != 0;
        let ep_capture = flags == MOVE_EP_CAPTURE;
        if capture && !ep_capture {
            let capture_piece = self.get_piece_64(to);
            self.hash ^= get_hash_value(capture_piece & PIECE_MASK, !self.white_to_move, to, hash_values);
            rollback.captured_pieces.push(capture_piece);
            self.game_stage -= GAME_STAGE_VALUES[(capture_piece & PIECE_MASK) as usize];
        }

        rollback.ep_index.push(self.en_passant_target_square_index);
        rollback.castling_rights.push(self.castling_rights);
        rollback.halfmove_clocks.push(self.halfmove_clock);

        let moved_piece = self.get_piece_64(from);
        if flags == MOVE_KING_CASTLE || flags == MOVE_QUEEN_CASTLE {
            let king_from = if self.white_to_move { 4 } else { 60 };
            let rook_to;
            let rook_from;
            let king_to;
            if flags == MOVE_KING_CASTLE {
                rook_to = king_from + 1;
                rook_from = king_from + 3;
                king_to = king_from + 2;
            } else {
                rook_to = king_from - 1;
                rook_from = king_from - 4;
                king_to = king_from - 2;
            }

            let color_flag = if self.white_to_move { 0 } else { COLOR_BLACK };
            self.write_piece(PIECE_NONE, king_from);
            self.hash ^= get_hash_value(PIECE_KING, self.white_to_move, king_from, hash_values);
            self.write_piece(PIECE_NONE, rook_from);
            self.hash ^= get_hash_value(PIECE_ROOK, self.white_to_move, rook_from, hash_values);
            self.write_piece(PIECE_KING | color_flag, king_to);
            self.hash ^= get_hash_value(PIECE_KING, self.white_to_move, king_to, hash_values);
            self.write_piece(PIECE_ROOK | color_flag, rook_to);
            self.hash ^= get_hash_value(PIECE_ROOK, self.white_to_move, rook_to, hash_values);
        } else if flags & MOVE_FLAG_PROMOTION != 0 {
            let color_flag = if self.white_to_move { 0 } else { COLOR_BLACK };
            let promo_value = (flags as u8) & 3;
            // +2 converts promo code to piece code
            let promo_to_piece = color_flag | (promo_value + 2);

            self.write_piece(PIECE_NONE, from);
            self.hash ^= get_hash_value(PIECE_PAWN, self.white_to_move, from, hash_values);
            self.write_piece(promo_to_piece, to);
            self.hash ^= get_hash_value(promo_value + 2, self.white_to_move, to, hash_values);
            self.game_stage += GAME_STAGE_VALUES[(promo_value + 2) as usize];
            self.game_stage -= GAME_STAGE_VALUES[PIECE_PAWN as usize];
        } else {
            if ep_capture {
                let diff = (to as isize).checked_sub_unsigned(from).unwrap();
                if diff == 7 || diff == -9 {
                    self.write_piece(PIECE_NONE, from - 1);
                    self.hash ^= get_hash_value(PIECE_PAWN, !self.white_to_move, from - 1, hash_values);
                } else {
                    self.write_piece(PIECE_NONE, from + 1);
                    self.hash ^= get_hash_value(PIECE_PAWN, !self.white_to_move, from + 1, hash_values);
                }
                self.game_stage -= GAME_STAGE_VALUES[PIECE_PAWN as usize];
            }

            let moved_piece_val = moved_piece & PIECE_MASK;
            self.write_piece(PIECE_NONE, from);
            self.hash ^= get_hash_value(moved_piece_val, self.white_to_move, from, hash_values);
            self.write_piece(moved_piece, to);
            self.hash ^= get_hash_value(moved_piece_val, self.white_to_move, to, hash_values);
        }

        if self.en_passant_target_square_index.is_some() {
            let file = file_8x8(self.en_passant_target_square_index.unwrap());
            self.hash ^= hash_values[HASH_VALUES_EP_FILE_IDX + file as usize];
        }

        let double_pawn_push = flags == MOVE_DOUBLE_PAWN;
        if double_pawn_push {
            let ep_index = from
                .checked_add_signed(if self.white_to_move { 8 } else { -8 })
                .unwrap() as u8;
            let file = file_8x8(ep_index);
            self.en_passant_target_square_index = Some(ep_index);
            self.hash ^= hash_values[HASH_VALUES_EP_FILE_IDX + file as usize];
        } else {
            self.en_passant_target_square_index = None;
        }

        if self.castling_rights != 0 {
            // potential optimization: match statement?
            if from == 0 || to == 0 {
                check_and_disable_castling(self, CastlingValue::WhiteQueen, hash_values);
            } else if from == 4 {
                check_and_disable_castling(self, CastlingValue::WhiteQueen, hash_values);
                check_and_disable_castling(self, CastlingValue::WhiteKing, hash_values);
            } else if from == 7 || to == 7 {
                check_and_disable_castling(self, CastlingValue::WhiteKing, hash_values);
            } else if from == 56 || to == 56 {
                check_and_disable_castling(self, CastlingValue::BlackQueen, hash_values);
            } else if from == 60 {
                check_and_disable_castling(self, CastlingValue::BlackQueen, hash_values);
                check_and_disable_castling(self, CastlingValue::BlackKing, hash_values);
            } else if from == 63 || to == 63 {
                check_and_disable_castling(self, CastlingValue::BlackKing, hash_values);
            }
        }

        if capture || moved_piece & PIECE_MASK == PIECE_PAWN {
            self.halfmove_clock = 0;
        } else {
            self.halfmove_clock += 1;
        }

        if !self.white_to_move {
            self.fullmove_counter += 1;
        }
        self.white_to_move = !self.white_to_move;
        self.hash ^= hash_values[HASH_VALUES_BLACK_TO_MOVE_IDX];

        self.repetitions.make_move(*r#move, self.hash);
    }

    pub fn unmake_move(&mut self, r#move: &Move, rollback: &mut MoveRollback) {
        self.repetitions.unmake_move(self.hash);

        let from = r#move.from() as usize;
        let to = r#move.to() as usize;
        let flags = r#move.flags();
        let hash_values = &*HASH_VALUES;

        let moved_piece = self.get_piece_64(to);

        let capture = (flags & MOVE_FLAG_CAPTURE) != 0;
        let ep_capture = flags == MOVE_EP_CAPTURE;
        if capture && !ep_capture {
            let captured_piece = rollback.captured_pieces.pop().unwrap();
            let moved_piece_val = moved_piece & PIECE_MASK;
            self.write_piece(captured_piece, to);
            self.hash ^= get_hash_value(captured_piece & PIECE_MASK, self.white_to_move, to, hash_values);
            self.hash ^= get_hash_value(moved_piece_val, !self.white_to_move, to, hash_values);
            self.game_stage += GAME_STAGE_VALUES[(captured_piece & PIECE_MASK) as usize];

            if flags & MOVE_FLAG_PROMOTION == 0 {
                self.write_piece(moved_piece, from);
                self.hash ^= get_hash_value(moved_piece_val, !self.white_to_move, from, hash_values);
            } else {
                let color_flag = if self.white_to_move { COLOR_BLACK } else { 0 };
                self.write_piece(PIECE_PAWN | color_flag, from);
                self.hash ^= get_hash_value(PIECE_PAWN, !self.white_to_move, from, hash_values);
                self.game_stage -= GAME_STAGE_VALUES[(moved_piece & PIECE_MASK) as usize];
                self.game_stage += GAME_STAGE_VALUES[PIECE_PAWN as usize];
            }
        } else if flags == MOVE_KING_CASTLE || flags == MOVE_QUEEN_CASTLE {
            let king_from;
            let rook_to;
            let rook_from;
            let king_to = to;
            if flags == MOVE_KING_CASTLE {
                rook_from = king_to + 1;
                rook_to = king_to - 1;
                king_from = king_to - 2;
            } else {
                rook_from = king_to - 2;
                rook_to = king_to + 1;
                king_from = king_to + 2;
            }

            let color_flag = if self.white_to_move { COLOR_BLACK } else { 0 };
            self.write_piece(PIECE_NONE, king_to);
            self.hash ^= get_hash_value(PIECE_KING, !self.white_to_move, king_to, hash_values);
            self.write_piece(PIECE_NONE, rook_to);
            self.hash ^= get_hash_value(PIECE_ROOK, !self.white_to_move, rook_to, hash_values);
            self.write_piece(PIECE_KING | color_flag, king_from);
            self.hash ^= get_hash_value(PIECE_KING, !self.white_to_move, king_from, hash_values);
            self.write_piece(PIECE_ROOK | color_flag, rook_from);
            self.hash ^= get_hash_value(PIECE_ROOK, !self.white_to_move, rook_from, hash_values);
        } else {
            if ep_capture {
                let opponent_color = if self.white_to_move { 0 } else { COLOR_BLACK };
                let diff = (to as isize).checked_sub_unsigned(from).unwrap();
                if diff == 7 || diff == -9 {
                    self.write_piece(PIECE_PAWN | opponent_color, from - 1);
                    self.hash ^= get_hash_value(PIECE_PAWN, self.white_to_move, from - 1, hash_values);
                } else {
                    self.write_piece(PIECE_PAWN | opponent_color, from + 1);
                    self.hash ^= get_hash_value(PIECE_PAWN, self.white_to_move, from + 1, hash_values);
                }
                self.game_stage += GAME_STAGE_VALUES[PIECE_PAWN as usize];
            }

            let moved_piece_val = moved_piece & PIECE_MASK;
            self.write_piece(PIECE_NONE, to);
            self.hash ^= get_hash_value(moved_piece_val, !self.white_to_move, to, hash_values);

            if flags & MOVE_FLAG_PROMOTION == 0 {
                self.write_piece(moved_piece, from);
                self.hash ^= get_hash_value(moved_piece_val, !self.white_to_move, from, hash_values);
            } else {
                let color_flag = if self.white_to_move { COLOR_BLACK } else { 0 };
                self.write_piece(PIECE_PAWN | color_flag, from);
                self.hash ^= get_hash_value(PIECE_PAWN, !self.white_to_move, from, hash_values);
                self.game_stage -= GAME_STAGE_VALUES[(moved_piece & PIECE_MASK) as usize];
                self.game_stage += GAME_STAGE_VALUES[PIECE_PAWN as usize];
            }
        }

        if self.en_passant_target_square_index.is_some() {
            let file = file_8x8(self.en_passant_target_square_index.unwrap());
            self.hash ^= hash_values[HASH_VALUES_EP_FILE_IDX + file as usize];
        }
        self.en_passant_target_square_index = rollback.ep_index.pop().unwrap();
        if self.en_passant_target_square_index.is_some() {
            let file = file_8x8(self.en_passant_target_square_index.unwrap());
            self.hash ^= hash_values[HASH_VALUES_EP_FILE_IDX + file as usize];
        }

        let old_castling_rights = self.castling_rights;
        self.castling_rights = rollback.castling_rights.pop().unwrap();
        if old_castling_rights != self.castling_rights {
            let diff = old_castling_rights ^ self.castling_rights;
            for i in 0..4usize {
                if diff & (1 << i) != 0 {
                    self.hash ^= hash_values[HASH_VALUES_CASTLE_BASE_IDX + i];
                }
            }
        }

        self.halfmove_clock = rollback.halfmove_clocks.pop().unwrap();

        if self.white_to_move {
            self.fullmove_counter -= 1;
        }
        self.white_to_move = !self.white_to_move;
        self.hash ^= hash_values[HASH_VALUES_BLACK_TO_MOVE_IDX];
    }

    /// Move must be a simple move piece from x to y. No captures, no pawn double pushes, no castling, etc.
    /// This is a simplified, specialized copy of unmake_move that must stay in sync.
    pub fn unmake_reversible_move_for_repetitions(&mut self, move_index: usize) {
        let m = self.repetitions.get_move_ref(move_index);
        let from = m.from() as usize;
        let to = m.to() as usize;
        let hash_values = &*HASH_VALUES;

        debug_assert_eq!(m.flags(), 0);

        let moved_piece = self.get_piece_64(to);

        let moved_piece_val = moved_piece & PIECE_MASK;
        self.write_piece(PIECE_NONE, to);
        self.hash ^= get_hash_value(moved_piece_val, !self.white_to_move, to, hash_values);
        self.write_piece(moved_piece, from);
        self.hash ^= get_hash_value(moved_piece_val, !self.white_to_move, from, hash_values);

        // Any move that resets this is irreversible so shouldn't need to check for underflow
        self.halfmove_clock -= 1;

        if self.white_to_move {
            self.fullmove_counter -= 1;
        }
        self.white_to_move = !self.white_to_move;
        self.hash ^= hash_values[HASH_VALUES_BLACK_TO_MOVE_IDX];
    }

    /// Move must be a simple move piece from x to y. No captures, no pawn double pushes, no castling, etc.
    /// This is a simplified, specialized copy of make_move that must stay in sync.
    pub fn make_reversible_move_for_repetitions(&mut self, move_index: usize) {
        let m = self.repetitions.get_move_ref(move_index);
        let from = m.from() as usize;
        let to = m.to() as usize;
        let hash_values = &*HASH_VALUES;

        debug_assert_eq!(m.flags(), 0);

        let moved_piece = self.get_piece_64(from);

        let moved_piece_val = moved_piece & PIECE_MASK;
        self.write_piece(PIECE_NONE, from);
        self.hash ^= get_hash_value(moved_piece_val, self.white_to_move, from, hash_values);
        self.write_piece(moved_piece, to);
        self.hash ^= get_hash_value(moved_piece_val, self.white_to_move, to, hash_values);

        self.halfmove_clock += 1;

        if !self.white_to_move {
            self.fullmove_counter += 1;
        }

        self.white_to_move = !self.white_to_move;
        self.hash ^= hash_values[HASH_VALUES_BLACK_TO_MOVE_IDX];
    }
}

// Not going to be super optimized probably and only support basic PGNs
pub fn pgn_to_moves(pgn: &str) -> Vec<Move> {
    let mut result = Vec::new();
    let mut board = Board::from_fen(STARTING_FEN);
    let mut rollback = MoveRollback::default();

    let parts = pgn.split_ascii_whitespace();
    let turn_pattern = Regex::new(r"[1-9][0-9]*\.").unwrap();
    let move_pattern = Regex::new(r"([nNbBrRqQkK]?)([a-f][1-8])").unwrap();
    for (i, part) in parts.enumerate() {
        if i % 3 == 0 {
            if !turn_pattern.is_match(part) {
                panic!("Expected '{part}' at index {i} to be a move number");
            }
            continue;
        } else {
            let Some(captures) = move_pattern.captures(part) else {
                panic!("Expected '{part}' at index {i} to be a move")
            };
            let piece_type = &captures[1];
            let square = &captures[2];
            let chars = square.chars();
        }
    }

    unimplemented!();

    result
}

pub fn square_indices_to_moves(indices: Vec<(u8, u8, Option<u16>)>) -> Vec<ScoredMove> {
    let mut result = Vec::new();
    let mut board = Board::from_fen(STARTING_FEN).unwrap();
    let mut rollback = MoveRollback::default();
    let mut history_table = [[[0; 64]; 6]; 2];

    for (i, r#move) in indices.iter().enumerate() {
        let mut moves = generate_legal_moves_without_history(&mut board);
        let Some(gen_move_pos) = moves.iter().position(|m| {
            if m.m.from() != r#move.0 as u16 || m.m.to() != r#move.1 as u16 {
                return false;
            }

            match r#move.2 {
                Some(p) => m.m.flags() & 0x03 == p,
                None => true,
            }
        }) else {
            debug!("{:?}", board);
            error!(
                "Requested move {} from {} {} to {} {} but it was not found in the board state",
                i + 1,
                r#move.0,
                index_8x8_to_pos_str(r#move.0),
                r#move.1,
                index_8x8_to_pos_str(r#move.1)
            );
            panic!("Requested move not found");
        };
        let gen_move = moves.swap_remove(gen_move_pos);

        board.make_move(&gen_move.m, &mut rollback);
        result.push(gen_move);
    }

    result
}

pub fn find_and_run_moves(board: &mut Board, indices: Vec<(u8, u8, Option<u16>)>) {
    let mut rollback = MoveRollback::default();
    let mut history_table = [[[0; 64]; 6]; 2];

    for (i, r#move) in indices.iter().enumerate() {
        let mut moves = generate_pseudo_legal_moves_without_history(board);
        let Some(gen_move_pos) = moves.iter().position(|m| {
            if m.m.from() != r#move.0 as u16 || m.m.to() != r#move.1 as u16 {
                return false;
            }

            match r#move.2 {
                Some(p) => m.m.flags() & FLAGS_MASK_PROMO == p,
                None => true,
            }
        }) else {
            error!(
                "Requested move {} from {} {} to {} {} but it was not found in the board state",
                i + 1,
                r#move.0,
                index_8x8_to_pos_str(r#move.0),
                r#move.1,
                index_8x8_to_pos_str(r#move.1)
            );
            error!("{:#?}", board);
            panic!("Requested move not found");
        };
        let gen_move = moves.swap_remove(gen_move_pos);

        // Pawn moves and captures are irreversible so we can clear previous game states.
        let mut clear_threefold_repetition = gen_move.m.flags() != 0;
        if !clear_threefold_repetition {
            let piece = board.get_piece_64(gen_move.m.from() as usize);
            clear_threefold_repetition = piece & PIECE_MASK == PIECE_PAWN;
        }

        if clear_threefold_repetition {
            board.repetitions.clear();
        }

        let (legal, _) = test_legality_and_maybe_make_move(board, gen_move.m, &mut rollback);
        if !legal {
            error!(
                "Requested move {} from {} {} to {} {}. Move is pseudo legal but not legal.",
                i + 1,
                r#move.0,
                index_8x8_to_pos_str(r#move.0),
                r#move.1,
                index_8x8_to_pos_str(r#move.1)
            );
            error!("{:#?}", board);
            panic!("Requested move is pseudo legal but not legal");
        }
    }
}

#[inline]
fn check_and_disable_castling(board: &mut Board, castling: CastlingValue, hash_values: &[u64; 781]) {
    if board.castling_rights & (1 << castling as u8) != 0 {
        board.castling_rights &= !(1 << castling as u8);
        board.hash ^= hash_values[HASH_VALUES_CASTLE_BASE_IDX + castling as usize];
    }
}
