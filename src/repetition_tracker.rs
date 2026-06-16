use crate::{
    board::{Board, PIECE_MASK, PIECE_PAWN},
    moves::Move,
    search::EMPTY_MOVE,
};

const TABLE_LOG_2_SIZE: usize = 14;
const MAX_MOVE_HISTORY: usize = 356;
const TABLE_MASK: u64 = (1 << TABLE_LOG_2_SIZE) - 1;
const MIN_REPETITIONS_FOR_DRAW: u8 = 2;

#[derive(Clone)]
pub struct RepetitionTracker {
    repetitions: [u8; 1 << TABLE_LOG_2_SIZE],
    move_history: [Move; MAX_MOVE_HISTORY],
    move_history_len: usize,
}

impl RepetitionTracker {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            repetitions: [0; 1 << TABLE_LOG_2_SIZE],
            move_history: [EMPTY_MOVE; MAX_MOVE_HISTORY],
            move_history_len: Default::default(),
        })
    }

    pub fn make_move(&mut self, m: Move, hash: u64) {
        if self.move_history_len >= MAX_MOVE_HISTORY {
            panic!(
                "RepetitionTracker make_move: move_history_len is {} which is greater than MAX_MOVE_HISTORY {MAX_MOVE_HISTORY}",
                self.move_history_len
            );
        }

        self.move_history[self.move_history_len] = m;
        self.move_history_len += 1;

        self.repetitions[(hash & TABLE_MASK) as usize] += 1;
    }

    pub fn unmake_move(&mut self, hash: u64) {
        if self.move_history_len == 0 {
            panic!("RepetitionTracker unmake_move: move_history_len is 0 which cannot go lower");
        }

        self.move_history_len -= 1;

        self.repetitions[(hash & TABLE_MASK) as usize] -= 1;
    }

    pub fn test_repetition(&self, board: &Board) -> bool {
        if self.repetitions[(board.hash & TABLE_MASK) as usize] >= MIN_REPETITIONS_FOR_DRAW {
            let mut check = true;
            let mut repetitions = 1;
            let target_hash = board.hash;
            let mut new_board = board.clone();

            let mut i = self.move_history_len - 1;
            loop {
                // if irreversible move
                if self.move_history[i].flags() != 0
                    || (new_board.get_piece_64(self.move_history[i].to() as usize) & PIECE_MASK) == PIECE_PAWN
                {
                    break;
                }

                new_board.unmake_reversible_move_for_repetitions(i, self);
                check = !check;

                if check && new_board.hash == target_hash {
                    repetitions += 1;
                    if repetitions == MIN_REPETITIONS_FOR_DRAW {
                        break;
                    }
                }

                // If out of moves
                if i == 0 {
                    break;
                }

                i -= 1;
            }

            repetitions >= MIN_REPETITIONS_FOR_DRAW
        } else {
            false
        }
    }

    pub fn clear(&mut self) {
        self.move_history_len = 0;

        self.repetitions = [0; 1 << TABLE_LOG_2_SIZE];
    }

    pub fn get_move(&self, move_index: usize) -> Move {
        debug_assert!(move_index < self.move_history_len);

        self.move_history[move_index]
    }

    pub fn add_start_position(&mut self, hash: u64) {
        self.repetitions[(hash & TABLE_MASK) as usize] += 1;
    }
}

impl PartialEq for RepetitionTracker {
    fn eq(&self, other: &Self) -> bool {
        if self.move_history_len != other.move_history_len {
            return false;
        }

        for i in 0..self.move_history_len {
            if self.move_history[i] != other.move_history[i] {
                return false;
            }
        }

        self.repetitions == other.repetitions
    }
}

#[cfg(test)]
mod repetition_tracker_tests {
    use crate::{STARTING_FEN, moves::MOVE_FLAG_CAPTURE};

    use super::*;

    #[test]
    pub fn repetition_from_starting_position() {
        let mut repetitions = RepetitionTracker::new();
        let mut board = Board::from_fen(STARTING_FEN, Some(&mut repetitions)).unwrap();

        board.make_move(Move::from_simple_long_algebraic_notation("g1f3", 0), &mut repetitions);
        assert!(!repetitions.test_repetition(&board));

        board.make_move(Move::from_simple_long_algebraic_notation("g8f6", 0), &mut repetitions);
        assert!(!repetitions.test_repetition(&board));

        board.make_move(Move::from_simple_long_algebraic_notation("f3g1", 0), &mut repetitions);
        assert!(!repetitions.test_repetition(&board));

        board.make_move(Move::from_simple_long_algebraic_notation("f6g8", 0), &mut repetitions);
        assert!(repetitions.test_repetition(&board));

        board.make_move(Move::from_simple_long_algebraic_notation("g1f3", 0), &mut repetitions);
        assert!(repetitions.test_repetition(&board));
    }

    #[test]
    pub fn no_false_positive_from_undoing_captures() {
        let mut repetitions = RepetitionTracker::new();
        let mut board = Board::from_fen("8/3B1p2/3n1k1p/8/2P4P/4K3/8/8 b - - 3 61", Some(&mut repetitions)).unwrap();

        board.make_move(
            Move::from_simple_long_algebraic_notation("d6c4", MOVE_FLAG_CAPTURE),
            &mut repetitions,
        );
        board.make_move(Move::from_simple_long_algebraic_notation("e3d4", 0), &mut repetitions);
        board.make_move(Move::from_simple_long_algebraic_notation("c4d6", 0), &mut repetitions);
        board.make_move(Move::from_simple_long_algebraic_notation("d4e3", 0), &mut repetitions);

        // Simulate a hash collision to force unmaking moves to test for repetition
        repetitions.repetitions[(board.hash & TABLE_MASK) as usize] += 1;
        // Prevent a panic due to underflow because more moves are being unmade than should be (release builds don't check for underflow so they wouldn't catch this)
        board.halfmove_clock += 1;

        // Bugged builds will actually fail at the flags != 0 debug assertion in unmake_reversible_move_for_repetitions, before testing this assertion
        assert!(!repetitions.test_repetition(&board));
    }
}
