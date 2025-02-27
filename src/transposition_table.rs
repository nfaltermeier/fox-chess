use log::error;

use crate::{evaluate::MATE_THRESHOLD, moves::Move};

pub enum TableType {
    Main,
    Quiescense,
}

#[derive(Copy, Clone)]
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
    eval: i16,
    pub draft: u8,
    pub empty: bool,
}

impl TTEntry {
    #[inline]
    pub fn new(hash: u64, important_move: Move, move_type: MoveType, eval: i16, draft: u8, ply: u8) -> Self {
        let mut tt_eval = eval;
        if tt_eval >= MATE_THRESHOLD {
            tt_eval += 10 * ply as i16;
        } else if tt_eval <= -MATE_THRESHOLD {
            tt_eval -= 10 * ply as i16;
        }

        Self {
            hash,
            important_move,
            move_type,
            eval: tt_eval,
            draft,
            empty: false,
        }
    }

    #[inline]
    pub fn get_eval(&self, ply: u8) -> i16 {
        let mut eval = self.eval;

        if eval >= MATE_THRESHOLD {
            eval -= 10 * ply as i16;
        } else if eval <= -MATE_THRESHOLD {
            eval += 10 * ply as i16;
        }

        eval
    }
}

impl Default for TTEntry {
    fn default() -> Self {
        TTEntry {
            hash: 0,
            important_move: Move { data: 0 },
            move_type: MoveType::FailHigh,
            eval: 0,
            draft: 0,
            empty: true,
        }
    }
}

pub struct TranspositionTable {
    main_table: Vec<TTEntry>,
    quiescense_table: Vec<TTEntry>,
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
            main_table: vec![TTEntry::default(); 1 << (size_log_2 - 1)],
            quiescense_table: vec![TTEntry::default(); 1 << (size_log_2 - 1)],
            key_mask: (1 << (size_log_2 - 1)) - 1,
            index_collisions: 0,
        }
    }

    pub fn get_entry(&mut self, key: u64, table: TableType) -> Option<TTEntry> {
        let index = key as usize & self.key_mask;
        let table = match table {
            TableType::Main => &self.main_table,
            TableType::Quiescense => &self.quiescense_table,
        };

        if let Some(entry) = table.get(index) {
            // Avoiding wasting an extra 8 bytes per entry by making the struct an Option
            if entry.empty {
                return None;
            }

            if entry.hash == key {
                return Some(*entry);
            } else {
                self.index_collisions += 1;
            }
        }

        None
    }

    pub fn store_entry(&mut self, val: TTEntry, table: TableType) {
        let index = val.hash as usize & self.key_mask;
        let table = match table {
            TableType::Main => &mut self.main_table,
            TableType::Quiescense => &mut self.quiescense_table,
        };

        table[index] = val;
    }

    pub fn clear(&mut self) {
        let default_entry = TTEntry::default();
        for i in 0..self.main_table.len() {
            self.main_table[i] = default_entry;
            self.quiescense_table[i] = default_entry;
        }

        self.index_collisions = 0;
    }
}
