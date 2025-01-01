use log::error;

use crate::moves::Move;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MoveType {
    FailHigh,
    Best,
    FailLow,
}

#[derive(Copy, Clone)]
pub struct TTEntry {
    pub hash: u64,
    pub important_move: Move,
    pub move_type: MoveType,
    pub eval: i16,
    pub move_num: u16,
}

impl Default for TTEntry {
    fn default() -> Self {
        TTEntry {
            hash: 0,
            important_move: Move { data: 0 },
            move_type: MoveType::FailHigh,
            eval: 0,
            move_num: 0,
        }
    }
}

pub struct TranspositionTable {
    table: Vec<TTEntry>,
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
            table: vec![TTEntry::default(); 1 << size_log_2],
            key_mask: (1 << size_log_2) - 1,
            index_collisions: 0,
        }
    }

    pub fn get_entry(&mut self, key: u64) -> Option<TTEntry> {
        let index = key as usize & self.key_mask;

        if let Some(entry) = self.table.get(index) {
            // Avoiding wasting an extra 8 bytes per entry by making the struct an Option
            if entry.move_num == 0 {
                return None;
            }

            if entry.hash == key {
                return Some(entry.clone());
            } else {
                self.index_collisions += 1;
            }
        }

        return None;
    }

    pub fn store_entry(&mut self, val: TTEntry) {
        let index = val.hash as usize & self.key_mask;
        self.table[index] = val;
    }

    pub fn clear(&mut self) {
        let default_entry = TTEntry::default();
        for i in 0..self.table.len() {
            self.table[i] = default_entry;
        }

        self.index_collisions = 0;
    }
}
