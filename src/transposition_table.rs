use std::{
    fs::File,
    io::{BufReader, BufWriter, Read, Write},
};

use bytemuck::{NoUninit, Pod, Zeroable, cast_slice};
use log::error;
use serde::{Deserialize, Serialize};

use crate::{evaluate::MATE_THRESHOLD, moves::Move};

#[derive(Copy, Clone, PartialEq, Eq, Serialize, Deserialize, NoUninit)]
#[repr(u8)]
pub enum MoveType {
    FailHigh = 0,
    Best,
    FailLow,
}

#[derive(Serialize, Deserialize)]
#[repr(C)]
pub struct TranspositionTable {
    table: Vec<TwoTierEntry>,
    key_mask: usize,
}

#[repr(C)]
#[derive(Copy, Clone, Default, Serialize, Deserialize, Pod, Zeroable)]
pub struct TwoTierEntry {
    pub always_replace: TTEntry,
    pub depth_first: TTEntry,
}

#[repr(C)]
#[derive(Copy, Clone, Serialize, Deserialize, Pod, Zeroable)]
pub struct TTEntry {
    pub hash: u64,
    pub important_move: Move,
    age: u8,
    pub move_type: u8,
    score: i16,
    pub draft: u8,
    pub empty: u8,
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
            empty: 0,
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
            empty: 1,
        }
    }
}

impl TranspositionTable {
    /// Panics if size_log_2 is less than 2
    pub fn new(size_log_2: u8) -> TranspositionTable {
        if size_log_2 < 10 {
            error!("TranspositionTable size_log_2 must be at least 10");
            panic!("TranspositionTable size_log_2 must be at least 10");
        }

        TranspositionTable {
            table: vec![TwoTierEntry::default(); 1 << (size_log_2 - 1)],
            key_mask: (1 << (size_log_2 - 1)) - 1,
        }
    }

    pub fn get_entry(&mut self, key: u64, search_starting_fullmove: u8) -> Option<TTEntry> {
        let index = key as usize & self.key_mask;

        if let Some(entry) = self.table.get_mut(index) {
            // Avoiding wasting an extra 8 bytes per entry by making the struct an Option
            if !entry.depth_first.empty != 0 && entry.depth_first.hash == key {
                entry.depth_first.age = search_starting_fullmove % 4;
                return Some(entry.depth_first);
            }

            if !entry.always_replace.empty != 0 && entry.always_replace.hash == key {
                entry.always_replace.age = search_starting_fullmove % 4;
                return Some(entry.always_replace);
            }
        }

        None
    }

    pub fn store_entry(&mut self, val: TTEntry) {
        let index = val.hash as usize & self.key_mask;

        if let Some(entry) = self.table.get_mut(index) {
            if entry.depth_first.empty != 0 || entry.depth_first.age != val.age || entry.depth_first.draft <= val.draft {
                entry.depth_first = val;
            } else {
                entry.always_replace = val;
            }
        }
    }

    pub fn clear(&mut self) {
        let default_entry = TwoTierEntry::default();
        self.table.iter_mut().for_each(|e| *e = default_entry);
    }

    pub fn hashfull(&self, search_starting_fullmove: u8) -> u16 {
        let target_age = search_starting_fullmove % 4;
        let mut count = 0;

        for entry in &self.table[0..500] {
            if !entry.depth_first.empty != 0 && entry.depth_first.age == target_age {
                count += 1;
            }

            if !entry.always_replace.empty != 0 && entry.always_replace.age == target_age {
                count += 1;
            }
        }

        count
    }

    // From chat gpt
    pub fn save_fast(&self, path: &str) {
        let mut file = BufWriter::new(File::create(path).unwrap());

        let table_bytes = cast_slice(&self.table);

        // Write header: lengths and metadata
        file.write_all(&(self.table.len() as u64).to_le_bytes()).unwrap();
        file.write_all(&(self.key_mask as u64).to_le_bytes()).unwrap();

        // Write raw bytes
        file.write_all(table_bytes).unwrap();
    }

    // From chat gpt
    pub fn load_fast(path: &str) -> Self {
        let mut file = BufReader::new(File::open(path).unwrap());

        let mut buf = [0u8; 8];

        // Read metadata
        file.read_exact(&mut buf).unwrap();
        let table_len = u64::from_le_bytes(buf) as usize;

        file.read_exact(&mut buf).unwrap();
        let key_mask = u64::from_le_bytes(buf) as usize;

        // Allocate vectors
        let mut table = vec![TwoTierEntry::zeroed(); table_len];

        // Read raw bytes directly into them
        let table_bytes = bytemuck::cast_slice_mut(&mut table);
        file.read_exact(table_bytes).unwrap();

        Self {
            table,
            key_mask,
        }
    }
}

impl From<u8> for MoveType {
    fn from(value: u8) -> Self {
        match value {
            0 => Self::FailHigh,
            1 => Self::Best,
            2 => Self::FailLow,
            _ => Self::FailLow,
        }
    }
}
