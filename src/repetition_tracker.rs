use std::fmt::Debug;

use log::error;

use crate::{
    board::{Board, HASH_VALUES, PIECE_MASK, PIECE_PAWN},
    move_generator::ENABLE_UNMAKE_MOVE_TEST,
    moves::Move,
};

// Repetition tracking is not needed when only doing quiescense for texel tuning
const TABLE_LOG_2_SIZE: usize = 0;
const MAX_MOVE_HISTORY: usize = 201;
const TABLE_MASK: u64 = (1 << TABLE_LOG_2_SIZE) - 1;
const MIN_REPETITIONS_FOR_DRAW: u8 = 2;

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

    pub fn test_repetition(board: &mut Board) -> bool {
        false
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
