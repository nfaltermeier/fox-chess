use std::sync::atomic::Ordering;

use bytemuck::{Pod, Zeroable, cast};
use log::error;
use portable_atomic::AtomicU128;

use crate::{evaluate::MATE_THRESHOLD, moves::Move};

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum MoveType {
    FailHigh = 0,
    Best,
    FailLow,
}

pub struct TranspositionTable {
    table: Vec<TwoTierEntry>,
    key_mask: usize,
}

#[derive(Default)]
struct TwoTierEntry {
    pub always_replace: AtomicU128,
    pub depth_first: AtomicU128,
}

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
pub struct TTEntry {
    pub hash: u64,
    pub important_move: Move,
    score: i16,
    age: u8,
    move_type: u8,
    pub draft: u8,
    occupied: u8,
}

impl TTEntry {
    #[inline]
    pub fn new(
        hash: u64,
        important_move: Move,
        move_type: MoveType,
        score: i16,
        draft: u8,
        ply: u8,
        search_starting_fullmove: u8,
    ) -> Self {
        const {
            assert!(size_of::<TTEntry>() == 16);
        }

        let mut tt_score = score;
        if tt_score >= MATE_THRESHOLD {
            tt_score += 10 * ply as i16;
        } else if tt_score <= -MATE_THRESHOLD {
            tt_score -= 10 * ply as i16;
        }

        Self {
            hash,
            important_move,
            age: search_starting_fullmove % 4,
            move_type: move_type as u8,
            score: tt_score,
            draft,
            occupied: 1,
        }
    }

    #[inline]
    pub fn get_score(&self, ply: u8) -> i16 {
        let mut score = self.score;

        if score >= MATE_THRESHOLD {
            score -= 10 * ply as i16;
        } else if score <= -MATE_THRESHOLD {
            score += 10 * ply as i16;
        }

        score
    }

    pub fn get_move_type(&self) -> u8 {
        self.move_type
    }
}

impl Default for TTEntry {
    fn default() -> Self {
        TTEntry {
            hash: 0,
            important_move: Move { data: 0 },
            age: 0,
            move_type: MoveType::FailHigh as u8,
            score: 0,
            draft: 0,
            occupied: 0,
        }
    }
}

impl TranspositionTable {
    /// Panics if size_log_2 is less than 10
    pub fn new(size_log_2: u8) -> TranspositionTable {
        if size_log_2 < 10 {
            error!("TranspositionTable size_log_2 must be at least 10");
            panic!("TranspositionTable size_log_2 must be at least 10");
        }

        let capacity = 1 << (size_log_2 - 1);
        let mut vec = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            vec.push(TwoTierEntry::default());
        }

        TranspositionTable {
            table: vec,
            key_mask: (1 << (size_log_2 - 1)) - 1,
        }
    }

    pub fn get_entry(&self, key: u64, search_starting_fullmove: u8) -> Option<TTEntry> {
        let index = key as usize & self.key_mask;

        if let Some(entry) = self.table.get(index) {
            let mut depth_first: TTEntry = cast(entry.depth_first.load(Ordering::Relaxed));
            // Avoiding wasting an extra 8 bytes per entry by making the struct an Option
            if depth_first.occupied != 0 && depth_first.hash == key {
                if depth_first.age != search_starting_fullmove % 4 {
                    depth_first.age = search_starting_fullmove % 4;
                    entry.depth_first.store(cast(depth_first), Ordering::Relaxed);
                }

                return Some(depth_first);
            }

            let mut always_replace: TTEntry = cast(entry.always_replace.load(Ordering::Relaxed));
            if always_replace.occupied != 0 && always_replace.hash == key {
                if always_replace.age != search_starting_fullmove % 4 {
                    always_replace.age = search_starting_fullmove % 4;
                    entry.always_replace.store(cast(always_replace), Ordering::Relaxed);
                }

                return Some(always_replace);
            }
        }

        None
    }

    pub fn store_entry(&self, val: TTEntry) {
        let index = val.hash as usize & self.key_mask;

        if let Some(entry) = self.table.get(index) {
            let depth_first: TTEntry = cast(entry.depth_first.load(Ordering::Relaxed));
            if depth_first.occupied == 0 || depth_first.age != val.age || depth_first.draft <= val.draft {
                TranspositionTable::replace_entry(&entry.depth_first, depth_first, val);
            } else {
                TranspositionTable::replace_entry(
                    &entry.always_replace,
                    cast(entry.always_replace.load(Ordering::Relaxed)),
                    val,
                );
            }
        }
    }

    fn replace_entry(entry: &AtomicU128, old_val: TTEntry, mut val: TTEntry) {
        if val.move_type == MoveType::FailLow as u8
            && old_val.move_type != MoveType::FailLow as u8
            && old_val.hash == val.hash
        {
            val.important_move = old_val.important_move;
        }

        entry.store(cast(val), Ordering::Relaxed);
    }

    pub fn clear(&mut self) {
        self.table.iter_mut().for_each(|e| *e = TwoTierEntry::default());
    }

    pub fn hashfull(&self, search_starting_fullmove: u8) -> u16 {
        let target_age = search_starting_fullmove % 4;
        let mut count = 0;

        for entry in &self.table[0..500] {
            let depth_first: TTEntry = cast(entry.depth_first.load(Ordering::Relaxed));
            if depth_first.occupied != 0 && depth_first.age == target_age {
                count += 1;
            }

            let always_replace: TTEntry = cast(entry.always_replace.load(Ordering::Relaxed));
            if always_replace.occupied != 0 && always_replace.age == target_age {
                count += 1;
            }
        }

        count
    }
}

impl From<u8> for MoveType {
    fn from(value: u8) -> Self {
        match value {
            0 => Self::FailHigh,
            1 => Self::Best,
            2 => Self::FailLow,
            _ => panic!("Invalid value in from(u8) -> MoveType"),
        }
    }
}
