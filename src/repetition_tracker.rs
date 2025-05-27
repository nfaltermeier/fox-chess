use std::fmt::Debug;

use log::{debug, error};

use crate::{
    board::{Board, HASH_VALUES, PIECE_MASK, PIECE_PAWN},
    move_generator::ENABLE_UNMAKE_MOVE_TEST,
    moves::Move,
};

// Repetition tracking is not needed when only doing quiescense for texel tuning
const TABLE_LOG_2_SIZE: usize = 0;
const MAX_MOVE_HISTORY: usize = 201;
const TABLE_MASK: u64 = (1 << TABLE_LOG_2_SIZE) - 1;

#[derive(Clone, Eq)]
pub struct RepetitionTracker {
    repetitions: [u8; 1 << TABLE_LOG_2_SIZE],
    move_history: [Move; MAX_MOVE_HISTORY],
    move_history_len: usize,
}

impl RepetitionTracker {
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

    pub fn test_threefold_repetition(board: &mut Board) -> bool {
        if board.repetitions.repetitions[(board.hash & TABLE_MASK) as usize] >= 3 {
            let mut check = true;
            let mut repetitions = 0;
            let target_hash = board.hash;
            let original_move_history_len = board.repetitions.move_history_len;

            let mut board_copy = None;
            if ENABLE_UNMAKE_MOVE_TEST {
                board_copy = Some(board.clone());
            }

            let mut i = board.repetitions.move_history_len - 1;
            loop {
                if check && board.hash == target_hash {
                    repetitions += 1;
                    if repetitions == 3 {
                        break;
                    }
                }

                // If irreversible move or out of moves
                if i == 0
                    || board.repetitions.move_history[i].flags() != 0
                    || (board.get_piece_64(board.repetitions.move_history[i].to() as usize) & PIECE_MASK) == PIECE_PAWN
                {
                    break;
                }

                board.unmake_reversible_move_for_repetitions(i);

                i -= 1;
                check = !check;
            }

            for move_index in (i + 1)..original_move_history_len {
                board.make_reversible_move_for_repetitions(move_index);
            }

            if ENABLE_UNMAKE_MOVE_TEST && board_copy.as_ref().unwrap() != board {
                error!("test_repetition did not properly undo board changes");

                let board_copied = board_copy.as_ref().unwrap();
                if board_copied.hash != board.hash {
                    let hash_values = &*HASH_VALUES;
                    for (i, v) in hash_values.iter().enumerate() {
                        if board_copied.hash ^ v == board.hash {
                            debug!("make/unmake differs by value {i} of HASH_VALUES in hash");
                        }
                    }
                }

                assert_eq!(board_copy.as_ref().unwrap(), board);
            }

            repetitions >= 3
        } else {
            false
        }
    }

    pub fn clear(&mut self) {
        self.move_history_len = 0;

        self.repetitions = [0; 1 << TABLE_LOG_2_SIZE];
    }

    pub fn get_move_ref(&self, move_index: usize) -> &Move {
        debug_assert!(move_index < self.move_history_len);

        &self.move_history[move_index]
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

impl Default for RepetitionTracker {
    fn default() -> Self {
        Self {
            repetitions: [0; 1 << TABLE_LOG_2_SIZE],
            move_history: [Move::new(0, 0, 0); MAX_MOVE_HISTORY],
            move_history_len: Default::default(),
        }
    }
}

impl Debug for RepetitionTracker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // f.debug_struct("RepetitionTracker").field("repetitions", &self.repetitions).field("move_history", &self.move_history).field("move_history_len", &self.move_history_len).finish()
        write!(f, "TODO: debug formatting for RepetitionTracker")
    }
}
