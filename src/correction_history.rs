use std::alloc::{Layout, alloc_zeroed, handle_alloc_error};

use crate::board::Board;

const CORRECTION_HISTORY_SIZE: u64 = 2 << 13;
const CORRECTION_HISTORY_LIMIT: i32 = 1024;

type CorrectionHistoryTable = [[i16; 2]; CORRECTION_HISTORY_SIZE as usize];

pub struct CorrectionHistoryTables {
    pawn: CorrectionHistoryTable,
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

    pub fn get_adjustment(&self, board: &Board) -> i16 {
        let side = if board.white_to_move { 0 } else { 1 };

        let pawn = self.pawn[(board.pawn_hash % CORRECTION_HISTORY_SIZE) as usize][side];

        pawn / 8
    }

    pub fn update_history(&mut self, board: &Board, diff: i16, draft: u8) {
        let side = if board.white_to_move { 0 } else { 1 };

        let bonus = (diff as i32 * draft as i32 / 8).clamp(-CORRECTION_HISTORY_LIMIT / 4, CORRECTION_HISTORY_LIMIT / 4);

        let pawn = &mut self.pawn[(board.pawn_hash % CORRECTION_HISTORY_SIZE) as usize][side];
        *pawn += (bonus - ((*pawn as i32) * bonus.abs() / CORRECTION_HISTORY_LIMIT)) as i16;
    }
}
