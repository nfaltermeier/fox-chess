use std::{fs::File, io::{BufReader, BufWriter, Read, Write}};

use bytemuck::{cast_slice, Pod, Zeroable};
use log::error;
use serde::{Deserialize, Serialize};

use crate::{evaluate::MATE_THRESHOLD, moves::Move};

pub enum TableType {
    Main,
    Quiescense,
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum MoveType {
    FailHigh = 0,
    Best,
    FailLow,
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
    score: i16,
    pub draft: u8,
    pub empty: u8,
    /// Bottom 2 bits: age, top 2 bits: MoveType
    packed: u8,
    padding: u8,
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
        search_starting_halfmove: u8,
    ) -> Self {
        let mut tt_score = score;
        if tt_score >= MATE_THRESHOLD {
            tt_score += 10 * ply as i16;
        } else if tt_score <= -MATE_THRESHOLD {
            tt_score -= 10 * ply as i16;
        }

        let mut packed = search_starting_halfmove % 4;
        packed |= (move_type as u8) << 6;

        Self {
            hash,
            important_move,
            packed,
            score: tt_score,
            draft,
            empty: 0,
            padding: 0,
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

    #[inline]
    pub fn get_move_type(&self) -> MoveType {
        MoveType::from(self.packed >> 6)
    }

    #[inline]
    pub fn get_age(&self) -> u8 {
        self.packed & 0b11
    }

    /// Age should be 0..3
    pub fn set_age(&mut self, age: u8) {
        debug_assert!(age <= 3);

        self.packed &= !0b11;
        self.packed |= age;
    }
}

impl Default for TTEntry {
    fn default() -> Self {
        TTEntry {
            hash: 0,
            important_move: Move { data: 0 },
            packed: 0xFF,
            score: 0,
            draft: 0,
            empty: 1,
            padding: 0,
        }
    }
}

#[derive(Serialize, Deserialize)]
#[repr(C)]
pub struct TranspositionTable {
    main_table: Vec<TwoTierEntry>,
    quiescense_table: Vec<TwoTierEntry>,
    key_mask: usize,
    pub index_collisions: u64,
}

#[derive(Serialize, Deserialize)]
pub struct BinaryTranspositionTable {
    main_table: Vec<u8>,
    quiescense_table: Vec<u8>,
    key_mask: usize,
    pub index_collisions: u64,
}

impl TranspositionTable {
    pub fn new(size_log_2: u8) -> TranspositionTable {
        if size_log_2 == 0 {
            error!("Size of 0 given for TranspositionTable");
            panic!("Size of 0 given for TranspositionTable");
        }

        TranspositionTable {
            main_table: vec![TwoTierEntry::default(); 1 << (size_log_2 - 2)],
            quiescense_table: vec![TwoTierEntry::default(); 1 << (size_log_2 - 2)],
            key_mask: (1 << (size_log_2 - 2)) - 1,
            index_collisions: 0,
        }
    }

    pub fn get_entry(&mut self, key: u64, table: TableType, search_starting_halfmove: u8) -> Option<TTEntry> {
        let index = key as usize & self.key_mask;
        let table = match table {
            TableType::Main => &mut self.main_table,
            TableType::Quiescense => &mut self.quiescense_table,
        };

        if let Some(entry) = table.get_mut(index) {
            // Avoiding wasting an extra 8 bytes per entry by making the struct an Option
            if !entry.depth_first.empty != 0 && entry.depth_first.hash == key {
                entry.depth_first.set_age(search_starting_halfmove % 4);
                return Some(entry.depth_first);
            }

            if !entry.always_replace.empty != 0 && entry.always_replace.hash == key {
                return Some(entry.always_replace);
            }

            // Technically should check for emptyness but eh
            self.index_collisions += 1;
        }

        None
    }

    pub fn store_entry(&mut self, val: TTEntry, table: TableType, force_overwrite: bool) {
        let index = val.hash as usize & self.key_mask;
        let table = match table {
            TableType::Main => &mut self.main_table,
            TableType::Quiescense => &mut self.quiescense_table,
        };

        if let Some(entry) = table.get_mut(index) {
            if force_overwrite
                || entry.depth_first.empty != 0
                || entry.depth_first.get_age() != val.get_age()
                || entry.depth_first.draft <= val.draft
            {
                entry.depth_first = val;
            } else {
                entry.always_replace = val;
            }
        }
    }

    pub fn clear(&mut self) {
        let default_entry = TwoTierEntry::default();
        for i in 0..self.main_table.len() {
            self.main_table[i] = default_entry;
            self.quiescense_table[i] = default_entry;
        }

        self.index_collisions = 0;
    }

    // From chat gpt
    pub fn save_fast(&self, path: &str) {
        let mut file = BufWriter::new(File::create(path).unwrap());

        let main_bytes = cast_slice(&self.main_table);
        let quies_bytes = cast_slice(&self.quiescense_table);

        // Write header: lengths and metadata
        file.write_all(&(self.main_table.len() as u64).to_le_bytes()).unwrap();
        file.write_all(&(self.quiescense_table.len() as u64).to_le_bytes()).unwrap();
        file.write_all(&(self.key_mask as u64).to_le_bytes()).unwrap();
        file.write_all(&self.index_collisions.to_le_bytes()).unwrap();

        // Write raw bytes
        file.write_all(main_bytes).unwrap();
        file.write_all(quies_bytes).unwrap();
    }

    // From chat gpt
    pub fn load_fast(path: &str) -> Self {
        let mut file = BufReader::new(File::open(path).unwrap());

        let mut buf = [0u8; 8];

        // Read metadata
        file.read_exact(&mut buf).unwrap();
        let main_len = u64::from_le_bytes(buf) as usize;

        file.read_exact(&mut buf).unwrap();
        let quies_len = u64::from_le_bytes(buf) as usize;

        file.read_exact(&mut buf).unwrap();
        let key_mask = u64::from_le_bytes(buf) as usize;

        file.read_exact(&mut buf).unwrap();
        let index_collisions = u64::from_le_bytes(buf);

        // Allocate vectors
        let mut main_table = vec![TwoTierEntry::zeroed(); main_len];
        let mut quiescense_table = vec![TwoTierEntry::zeroed(); quies_len];

        // Read raw bytes directly into them
        let main_bytes = bytemuck::cast_slice_mut(&mut main_table);
        file.read_exact(main_bytes).unwrap();

        let quies_bytes = bytemuck::cast_slice_mut(&mut quiescense_table);
        file.read_exact(quies_bytes).unwrap();

        Self {
            main_table,
            quiescense_table,
            key_mask,
            index_collisions,
        }
    }
}
