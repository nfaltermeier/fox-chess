use std::{mem::swap, sync::LazyLock};

use arrayvec::ArrayVec;

use crate::{
    bitboard::{BIT_SQUARES, SQUARES_BETWEEN, bitscan_forward_and_reset, lookup_king_attack, lookup_knight_attack},
    board::{
        Board, HASH_VALUES_BLACK_TO_MOVE_IDX, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_QUEEN, PIECE_ROOK,
        ZOBRIST_HASH_VALUES, get_zobrist_hash_value,
    },
    magic_bitboard::{COMBINED_BISHOP_RAYS, COMBINED_ROOK_RAYS},
};

const TABLE_LOG_2_SIZE: usize = 14;
const TABLE_MASK: u64 = (1 << TABLE_LOG_2_SIZE) - 1;

static REPETITION_MOVES_DATA: LazyLock<Box<MovesCuckoo>> = LazyLock::new(|| {
    let mut result = MovesCuckoo::new();

    let zobrist_hashes = &*ZOBRIST_HASH_VALUES;

    for piece_type in [PIECE_KNIGHT, PIECE_BISHOP, PIECE_ROOK, PIECE_KING] {
        // exclude index 63 because I am only allowing moving to squares with a higher index than from, which is no squares for index 63.
        for from in 0..63 {
            match piece_type {
                PIECE_KNIGHT => {
                    let mut squares = lookup_knight_attack(from);
                    // Mask off bits equal to or lower than 1 << from so that to > from so that the reverse of a move is not also entered
                    squares &= !((BIT_SQUARES[from as usize] - 1) | BIT_SQUARES[from as usize]);
                    while squares != 0 {
                        let to = bitscan_forward_and_reset(&mut squares) as u8;

                        let hash = get_zobrist_hash_value(piece_type, true, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(piece_type, true, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                        let hash = get_zobrist_hash_value(piece_type, false, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(piece_type, false, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                    }
                }
                PIECE_BISHOP => {
                    let mut squares = COMBINED_BISHOP_RAYS[from as usize];
                    squares &= !((BIT_SQUARES[from as usize] - 1) | BIT_SQUARES[from as usize]);
                    while squares != 0 {
                        let to = bitscan_forward_and_reset(&mut squares) as u8;

                        let hash = get_zobrist_hash_value(piece_type, true, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(piece_type, true, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                        let hash = get_zobrist_hash_value(piece_type, false, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(piece_type, false, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);

                        let hash = get_zobrist_hash_value(PIECE_QUEEN, true, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(PIECE_QUEEN, true, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                        let hash = get_zobrist_hash_value(PIECE_QUEEN, false, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(PIECE_QUEEN, false, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                    }
                }
                PIECE_ROOK => {
                    let mut squares = COMBINED_ROOK_RAYS[from as usize];
                    squares &= !((BIT_SQUARES[from as usize] - 1) | BIT_SQUARES[from as usize]);
                    while squares != 0 {
                        let to = bitscan_forward_and_reset(&mut squares) as u8;

                        let hash = get_zobrist_hash_value(piece_type, true, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(piece_type, true, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                        let hash = get_zobrist_hash_value(piece_type, false, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(piece_type, false, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);

                        let hash = get_zobrist_hash_value(PIECE_QUEEN, true, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(PIECE_QUEEN, true, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                        let hash = get_zobrist_hash_value(PIECE_QUEEN, false, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(PIECE_QUEEN, false, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                    }
                }
                PIECE_KING => {
                    let mut squares = lookup_king_attack(from);
                    squares &= !((BIT_SQUARES[from as usize] - 1) | BIT_SQUARES[from as usize]);
                    while squares != 0 {
                        let to = bitscan_forward_and_reset(&mut squares) as u8;

                        let hash = get_zobrist_hash_value(piece_type, true, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(piece_type, true, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                        let hash = get_zobrist_hash_value(piece_type, false, from as usize, zobrist_hashes)
                            ^ get_zobrist_hash_value(piece_type, false, to as usize, zobrist_hashes)
                            ^ zobrist_hashes[HASH_VALUES_BLACK_TO_MOVE_IDX];
                        result.insert(hash, from, to);
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    result
});

#[derive(Clone, PartialEq)]
pub struct RepetitionTracker {
    repetitions: [u8; 1 << TABLE_LOG_2_SIZE],
    hashes: ArrayVec<u64, 356>,
}

struct MovesCuckoo {
    hashes: [u64; 8192],
    from: [u8; 8192],
    to: [u8; 8192],
}

impl RepetitionTracker {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            repetitions: [0; 1 << TABLE_LOG_2_SIZE],
            hashes: ArrayVec::new(),
        })
    }

    pub fn push_hash(&mut self, hash: u64) {
        self.hashes.push(hash);
        self.repetitions[(hash & TABLE_MASK) as usize] += 1;
    }

    pub fn pop_hash(&mut self) {
        let hash = self.hashes.pop().unwrap();
        self.repetitions[(hash & TABLE_MASK) as usize] -= 1;
    }

    pub fn position_has_repeated_times(&self, board: &Board, times: u8) -> bool {
        if board.moves_since_irreversible >= 4 && self.repetitions[(board.hash & TABLE_MASK) as usize] >= 2 {
            let mut left = board.moves_since_irreversible - 3;
            let mut occurrences = 1;
            for hash in self.hashes.iter().rev().skip(4).step_by(2) {
                if *hash == board.hash {
                    occurrences += 1;
                    if occurrences >= times {
                        return true;
                    }
                }

                if left <= 1 {
                    return false;
                }

                left -= 2;
            }

            false
        } else {
            false
        }
    }

    // Implementation based on https://web.archive.org/web/20180713113001/https://marcelk.net/2013-04-06/paper/upcoming-rep-v2.pdf
    pub fn test_has_upcoming_repetition(&self, board: &Board) -> bool {
        if board.moves_since_irreversible < 3 {
            return false;
        }

        let stm_hash = ZOBRIST_HASH_VALUES[HASH_VALUES_BLACK_TO_MOVE_IDX];

        let mut other = self.hashes_reversed_index(0)
            ^ self.hashes_reversed_index(1)
            ^ stm_hash;

        let moves_data = &**REPETITION_MOVES_DATA;

        // d is incremented at the start of the loop, so subtract 2 from the initial value and add 2 when doing bounds check
        let mut d = 1;
        while d + 2 <= board.moves_since_irreversible as usize {
            d += 2;
            other ^= self.hashes_reversed_index(d - 1)
                ^ self.hashes_reversed_index(d)
                ^ stm_hash;

            // The opponent's pieces must have reverted for us to be able to make a repetition
            if other != 0 {
                continue;
            }

            let diff = self.hashes_reversed_index(0) ^ self.hashes_reversed_index(d);

            // moves_data will contain the diff if the move is legal on an empty board and check if the move is obstructed on our current board
            if let Some((from, to)) = moves_data.contains(diff)
                && SQUARES_BETWEEN[from as usize][to as usize] & board.occupancy == 0
            {
                return true;
            }
        }

        false
    }

    /// Reverses the hashes indexing, so index 0 passed in corresponds to the current position (last entry in the hash stack)
    fn hashes_reversed_index(&self, r: usize) -> u64 {
        self.hashes[self.hashes.len() - 1 - r]
    }

    pub fn clear(&mut self) {
        self.hashes.clear();
        self.repetitions = [0; 1 << TABLE_LOG_2_SIZE];
    }
}

impl MovesCuckoo {
    fn new() -> Box<Self> {
        Box::new(MovesCuckoo {
            hashes: [0; 8192],
            from: [0; 8192],
            to: [0; 8192],
        })
    }

    fn insert(&mut self, mut hash: u64, mut from: u8, mut to: u8) {
        let mut index = Self::hash_1(hash);
        loop {
            swap(&mut self.hashes[index], &mut hash);
            swap(&mut self.from[index], &mut from);
            swap(&mut self.to[index], &mut to);

            if hash == 0 {
                break;
            }

            index = if index == Self::hash_1(hash) {
                Self::hash_2(hash)
            } else {
                Self::hash_1(hash)
            };
        }
    }

    fn contains(&self, hash: u64) -> Option<(u8, u8)> {
        let index = Self::hash_1(hash);
        if self.hashes[index] == hash {
            return Some((self.from[index], self.to[index]));
        }

        let index = Self::hash_2(hash);
        if self.hashes[index] == hash {
            return Some((self.from[index], self.to[index]));
        }

        None
    }

    fn hash_1(hash: u64) -> usize {
        ((hash >> 32) & 0x1fff) as usize
    }

    fn hash_2(hash: u64) -> usize {
        ((hash >> 48) & 0x1fff) as usize
    }
}

pub fn init_repetition_moves_data() {
    let _ = *REPETITION_MOVES_DATA;
}

#[cfg(test)]
mod repetition_tracker_tests {
    use std::sync::mpsc;

    use vampirc_uci::parse_with_unknown;

    use crate::{
        STARTING_FEN,
        board::{COLOR_BLACK, PIECE_MASK, PIECE_PAWN, Squares, piece_to_colored_letter},
        magic_bitboard::initialize_magic_bitboards,
        moves::{MOVE_FLAG_CAPTURE, Move},
        uci::UciInterface,
    };

    use super::*;

    #[test]
    pub fn repetition_from_starting_position() {
        let mut repetitions = RepetitionTracker::new();
        let mut board = Board::from_fen(STARTING_FEN, Some(&mut repetitions)).unwrap();

        board.make_move(
            Move::from_simple_long_algebraic_notation("g1f3", 0),
            &mut repetitions,
            None,
            None,
        );
        assert!(!repetitions.position_has_repeated_times(&board, 2));
        assert!(!repetitions.test_has_upcoming_repetition(&board));

        board.make_move(
            Move::from_simple_long_algebraic_notation("g8f6", 0),
            &mut repetitions,
            None,
            None,
        );
        assert!(!repetitions.position_has_repeated_times(&board, 2));
        assert!(!repetitions.test_has_upcoming_repetition(&board));

        board.make_move(
            Move::from_simple_long_algebraic_notation("f3g1", 0),
            &mut repetitions,
            None,
            None,
        );
        assert!(!repetitions.position_has_repeated_times(&board, 2));
        assert!(repetitions.test_has_upcoming_repetition(&board));

        board.make_move(
            Move::from_simple_long_algebraic_notation("f6g8", 0),
            &mut repetitions,
            None,
            None,
        );
        assert!(repetitions.position_has_repeated_times(&board, 2));
        assert!(repetitions.test_has_upcoming_repetition(&board));

        board.make_move(
            Move::from_simple_long_algebraic_notation("g1f3", 0),
            &mut repetitions,
            None,
            None,
        );
        assert!(repetitions.position_has_repeated_times(&board, 2));
        assert!(repetitions.test_has_upcoming_repetition(&board));
    }

    macro_rules! repetition_top_right_corner_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (piece, move_to) = $value;

                    let white_piece = piece & COLOR_BLACK == 0;
                    let piece_str = piece_to_colored_letter(piece).to_string();
                    let own_filler = piece_to_colored_letter(
                        if piece & PIECE_MASK != PIECE_KING {
                            PIECE_KING
                        } else {
                            PIECE_PAWN
                        } | if white_piece { 0 } else { COLOR_BLACK },
                    )
                    .to_string();
                    let other_king = piece_to_colored_letter(PIECE_KING | if white_piece { COLOR_BLACK } else { 0 }).to_string();
                    let color_to_move_str = if white_piece { "w" } else { "b" };

                    let fen = format!("7{piece_str}/8/2{own_filler}5/8/8/8/4{other_king}3/8 {color_to_move_str} - - 0 1");

                    let mut repetitions = RepetitionTracker::new();
                    let mut board = Board::from_fen(&fen, Some(&mut repetitions)).unwrap();

                    let mov = Move::new(Squares::H8 as u8, move_to as u8, 0);
                    board.make_move(mov, &mut repetitions, None, None);
                    assert!(!repetitions.position_has_repeated_times(&board, 2));
                    assert!(!repetitions.test_has_upcoming_repetition(&board));

                    let mov = Move::new(Squares::E2 as u8, Squares::F1 as u8, 0);
                    board.make_move(mov, &mut repetitions, None, None);
                    assert!(!repetitions.position_has_repeated_times(&board, 2));
                    assert!(!repetitions.test_has_upcoming_repetition(&board));

                    let mov = Move::new(move_to as u8, Squares::H8 as u8, 0);
                    board.make_move(mov, &mut repetitions, None, None);
                    assert!(!repetitions.position_has_repeated_times(&board, 2));
                    assert!(repetitions.test_has_upcoming_repetition(&board));

                    let mov = Move::new(Squares::F1 as u8, Squares::E2 as u8, 0);
                    board.make_move(mov, &mut repetitions, None, None);
                    assert!(repetitions.position_has_repeated_times(&board, 2));
                    assert!(repetitions.test_has_upcoming_repetition(&board));

                    let mov = Move::new(Squares::H8 as u8, move_to as u8, 0);
                    board.make_move(mov, &mut repetitions, None, None);
                    assert!(repetitions.position_has_repeated_times(&board, 2));
                    assert!(repetitions.test_has_upcoming_repetition(&board));
                }
            )*
        }
    }

    repetition_top_right_corner_tests! {
        repetition_top_right_corner_test_wq_moves_orthogonal: (PIECE_QUEEN, Squares::H4),
        repetition_top_right_corner_test_bq_moves_orthogonal: (PIECE_QUEEN | COLOR_BLACK, Squares::H4),
        repetition_top_right_corner_test_wq_moves_diagonal: (PIECE_QUEEN, Squares::D4),
        repetition_top_right_corner_test_bq_moves_diagonal: (PIECE_QUEEN | COLOR_BLACK, Squares::D4),
        repetition_top_right_corner_test_wr: (PIECE_ROOK, Squares::H4),
        repetition_top_right_corner_test_br: (PIECE_ROOK | COLOR_BLACK, Squares::H4),
        repetition_top_right_corner_test_wb: (PIECE_BISHOP, Squares::D4),
        repetition_top_right_corner_test_bb: (PIECE_BISHOP | COLOR_BLACK, Squares::D4),
        repetition_top_right_corner_test_wk: (PIECE_KING, Squares::G8),
        repetition_top_right_corner_test_bk: (PIECE_KING | COLOR_BLACK, Squares::G8),
        repetition_top_right_corner_test_wn: (PIECE_KNIGHT, Squares::G6),
        repetition_top_right_corner_test_bn: (PIECE_KNIGHT | COLOR_BLACK, Squares::G6),
    }

    #[test]
    pub fn no_false_positive_from_undoing_captures() {
        let mut repetitions = RepetitionTracker::new();
        let mut board = Board::from_fen("8/3B1p2/3n1k1p/8/2P4P/4K3/8/8 b - - 3 61", Some(&mut repetitions)).unwrap();

        board.make_move(
            Move::from_simple_long_algebraic_notation("d6c4", MOVE_FLAG_CAPTURE),
            &mut repetitions,
            None,
            None,
        );
        board.make_move(
            Move::from_simple_long_algebraic_notation("e3d4", 0),
            &mut repetitions,
            None,
            None,
        );
        board.make_move(
            Move::from_simple_long_algebraic_notation("c4d6", 0),
            &mut repetitions,
            None,
            None,
        );

        assert!(!repetitions.test_has_upcoming_repetition(&board));

        board.make_move(
            Move::from_simple_long_algebraic_notation("d4e3", 0),
            &mut repetitions,
            None,
            None,
        );

        // Simulate a hash collision to force unmaking moves to test for repetition
        repetitions.repetitions[(board.hash & TABLE_MASK) as usize] += 1;
        // Prevent a panic due to underflow because more moves are being unmade than should be (release builds don't check for underflow so they wouldn't catch this)
        board.halfmove_clock += 1;

        // Bugged builds will actually fail at the flags != 0 debug assertion in unmake_reversible_move_for_repetitions, before testing this assertion
        assert!(!repetitions.position_has_repeated_times(&board, 2));
        assert!(repetitions.test_has_upcoming_repetition(&board));

        // This is an actual repetition after the capture
        board.make_move(
            Move::from_simple_long_algebraic_notation("d6c4", 0),
            &mut repetitions,
            None,
            None,
        );
        assert!(repetitions.position_has_repeated_times(&board, 2));
        assert!(repetitions.test_has_upcoming_repetition(&board));
    }

    #[test]
    fn upcoming_rep_gamepos_1() {
        let uci_command = "position fen r2qkbnr/1pp2ppp/p2p4/3Pp3/3nP3/2NB4/PPP2PPP/R1BQK2R b KQkq - 1 8 moves g7g6"
            .to_owned()
            + " e1g1 f8g7 f2f4 e5f4 c1f4 g8h6 d1d2 h6g4 h2h3 g4e5 f4e3 c7c5 d5c6 d4c6 c3d5 e5d3 c2d3 e8g8 f1f2 f7f5 e4f5"
            + " f8f5 f2f5 g6f5 a1f1 d8f8 d5c7 a8c8 c7e6 f8f6 e6g7 g8g7 g2g4 c6d4 d2f2 d4c6 e3d2 g7g8 d2c3 f6g5 f2f5 g5f5"
            + " f1f5 c6e5 f5g5 g8f7 g1g2 b7b5 g5h5 f7g6 c3e5 d6e5 h5e5 c8c2 g2f3 c2d2 e5e6 g6g7 e6a6 d2d3 f3f4 d3h3 a6b6"
            + " h7h5 b6b5 h5g4 f4g4 h3h1 g4f4 g7f6 f4e3 f6e6 e3d4 e6d6 d4c3 h1h4 b5b4 h4h3 c3c4 d6c6 a2a3 h3f3 b4b8 f3f2"
            + " c4c3 f2f4 b8b4 f4f3 c3c4 f3g3 c4d4 g3g4 d4c3 g4g3 c3c4 g3f3 b4b8 f3f2 b2b4 f2f3 a3a4 f3f4 c4c3 c6c7 b8b5"
            + " f4f3 c3d4 f3f4 d4d5 f4f5 d5e4 f5b5 a4b5 c7b6 e4d4 b6b5 d4c3 b5a6 c3c4 a6b6 b4b5 b6c7 c4c5 c7b7 c5b4 b7b6"
            + " b4c4 b6c7 c4c5 c7b7 c5b4 b7b6 b4a4 b6b7 a4b3 b7b8 b3b4 b8a8 b4c4 a8b7 c4c5 b7c7 b5b6 c7b7 c5b5 b7b8 b5b4"
            + " b8a8 b4c4 a8b7 c4b5 b7b8 b5a4 b8c8 a4b4 c8b8 b4b5 b8b7 b5c5 b7b8 c5c6 b8c8 c6d5 c8b8 d5d4 b8b7 d4c5 b7b8"
            + " c5b4 b8a8 b4c4 a8b7 c4c5";

        initialize_magic_bitboards();

        let (_, stop_rx) = mpsc::channel::<()>();
        let mut uci = UciInterface::new(10, stop_rx);

        let messages = parse_with_unknown(&uci_command);
        uci.process_command(&uci_command, messages);
        let board = uci.get_board_copy().unwrap();

        // b7b8 is a repetition, which was the move played 5 moves ago when the current position repeated earlier
        assert!(uci.repetition_tracker().test_has_upcoming_repetition(&board));
    }
}
