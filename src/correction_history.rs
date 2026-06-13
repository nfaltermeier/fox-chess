use std::alloc::{Layout, alloc_zeroed, handle_alloc_error};

use crate::{
    board::{Board, COLOR_BLACK, PIECE_MASK, ZOBRIST_HASH_VALUES, get_zobrist_hash_value},
    search::{EMPTY_MOVE, SearchStack},
};

const CORRECTION_HISTORY_SIZE: u64 = 2 << 13;
const CORRECTION_HISTORY_LIMIT: i32 = 1024;

type CorrectionHistoryTable = [[i16; 2]; CORRECTION_HISTORY_SIZE as usize];

pub struct CorrectionHistoryTables {
    pawn: CorrectionHistoryTable,
    material: CorrectionHistoryTable,
    nonpawn_white: CorrectionHistoryTable,
    nonpawn_black: CorrectionHistoryTable,
    cont1: CorrectionHistoryTable,
}

impl CorrectionHistoryTables {
    pub fn new() -> Box<Self> {
        let layout = Layout::new::<CorrectionHistoryTables>();

        unsafe {
            // Safety: 0 is a valid value for i16 and CorrectionHistoryTables is a struct of multidimensional arrays of i16
            let mem = alloc_zeroed(layout);

            if mem.is_null() {
                handle_alloc_error(layout);
            }

            let typed_mem = mem.cast::<CorrectionHistoryTables>();
            Box::from_raw(typed_mem)
        }
    }

    pub fn get_adjustment(&self, board: &Board, ss: &Vec<SearchStack>, ply: u8) -> i16 {
        let side = if board.white_to_move { 0 } else { 1 };

        let pawn = self.pawn[(board.pawn_hash % CORRECTION_HISTORY_SIZE) as usize][side];
        let material = self.material[(board.material_hash % CORRECTION_HISTORY_SIZE) as usize][side];
        let nonpawn_white = self.nonpawn_white[(board.nonpawn_hashes[0] % CORRECTION_HISTORY_SIZE) as usize][side];
        let nonpawn_black = self.nonpawn_black[(board.nonpawn_hashes[1] % CORRECTION_HISTORY_SIZE) as usize][side];

        let cont1 = if ply > 1 && can_continuation_history(ss, ply, 2) {
            let zobrist_hash_values = &*ZOBRIST_HASH_VALUES;
            self.cont1[(get_continuation_hash(ss, ply, 2, zobrist_hash_values) % CORRECTION_HISTORY_SIZE) as usize]
                [side]
        } else {
            0
        };

        (pawn + material + nonpawn_white + nonpawn_black + cont1) / 24
    }

    pub fn update_history(&mut self, board: &Board, diff: i16, draft: u8, ss: &Vec<SearchStack>, ply: u8) {
        let side = if board.white_to_move { 0 } else { 1 };

        let bonus = (diff as i32 * draft as i32 / 8).clamp(-CORRECTION_HISTORY_LIMIT / 4, CORRECTION_HISTORY_LIMIT / 4);

        let pawn = &mut self.pawn[(board.pawn_hash % CORRECTION_HISTORY_SIZE) as usize][side];
        *pawn += (bonus - ((*pawn as i32) * bonus.abs() / CORRECTION_HISTORY_LIMIT)) as i16;

        let material = &mut self.material[(board.material_hash % CORRECTION_HISTORY_SIZE) as usize][side];
        *material += (bonus - ((*material as i32) * bonus.abs() / CORRECTION_HISTORY_LIMIT)) as i16;

        let nonpawn_white = &mut self.nonpawn_white[(board.nonpawn_hashes[0] % CORRECTION_HISTORY_SIZE) as usize][side];
        *nonpawn_white += (bonus - ((*nonpawn_white as i32) * bonus.abs() / CORRECTION_HISTORY_LIMIT)) as i16;

        let nonpawn_black = &mut self.nonpawn_black[(board.nonpawn_hashes[1] % CORRECTION_HISTORY_SIZE) as usize][side];
        *nonpawn_black += (bonus - ((*nonpawn_black as i32) * bonus.abs() / CORRECTION_HISTORY_LIMIT)) as i16;

        if ply > 1 && can_continuation_history(ss, ply, 2) {
            let zobrist_hash_values = &*ZOBRIST_HASH_VALUES;
            let cont1 = &mut self.cont1
                [(get_continuation_hash(ss, ply, 2, zobrist_hash_values) % CORRECTION_HISTORY_SIZE) as usize][side];
            *cont1 += (bonus - ((*cont1 as i32) * bonus.abs() / CORRECTION_HISTORY_LIMIT)) as i16;
        }
    }
}

fn get_continuation_hash(ss: &Vec<SearchStack>, ply: u8, ply_offset: u8, zobrist_hash_values: &[u64; 781]) -> u64 {
    let p1 = &ss[(ply - 1) as usize];
    let p2 = &ss[(ply - ply_offset) as usize];

    debug_assert_ne!(p1.get_mov(), EMPTY_MOVE);
    debug_assert_ne!(p1.get_moved_piece_type(), 0xFF);
    debug_assert_ne!(p2.get_mov(), EMPTY_MOVE);
    debug_assert_ne!(p2.get_moved_piece_type(), 0xFF);

    get_zobrist_hash_value(
        p1.get_moved_piece_type() & PIECE_MASK,
        p1.get_moved_piece_type() & COLOR_BLACK == 0,
        p1.get_mov().to() as usize,
        zobrist_hash_values,
    ) ^ get_zobrist_hash_value(
        p2.get_moved_piece_type() & PIECE_MASK,
        p2.get_moved_piece_type() & COLOR_BLACK == 0,
        p2.get_mov().to() as usize,
        zobrist_hash_values,
    )
}

/// Disallow continuation history for null moves
fn can_continuation_history(ss: &Vec<SearchStack>, ply: u8, ply_offset: u8) -> bool {
    let p1 = &ss[(ply - 1) as usize];
    let p2 = &ss[(ply - ply_offset) as usize];

    p1.get_mov() != EMPTY_MOVE && p2.get_mov() != EMPTY_MOVE
}
