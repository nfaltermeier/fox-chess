use std::alloc::{Layout, alloc_zeroed, handle_alloc_error};

use crate::{
    board::{Board, COLOR_BLACK, PIECE_MASK, ZOBRIST_HASH_VALUES, get_zobrist_hash_value},
    move_generator::{
        MOVE_SCORE_CONT_HISTORY_PLY1_MAX, MOVE_SCORE_CONT_HISTORY_PLY2_MAX, MOVE_SCORE_HISTORY_MAX,
        MOVE_SCORE_KILLER_1, MOVE_SCORE_KILLER_2, ScoredMove,
    },
    moves::Move,
    search::{EMPTY_MOVE, SearchStack},
};

const CORRECTION_HISTORY_SIZE: u64 = 2 << 13;
const CORRECTION_HISTORY_LIMIT: i32 = 1024;

type CorrectionHistoryTable = [[i16; 2]; CORRECTION_HISTORY_SIZE as usize];
pub type HistoryTable = [[[i16; 64]; 6]; 2];
pub type ContinuationHistoryTable = [[[[[i16; 64]; 6]; 64]; 6]; 2];
pub type MutRelevantContinuationHistories<'a> = [Option<&'a mut [[i16; 64]; 6]>; 2];
pub type RelevantContinuationHistories<'a> = [Option<&'a [[i16; 64]; 6]>; 2];

pub struct CorrectionHistoryTables {
    pawn: CorrectionHistoryTable,
    material: CorrectionHistoryTable,
    nonpawn_white: CorrectionHistoryTable,
    nonpawn_black: CorrectionHistoryTable,
    cont1: CorrectionHistoryTable,
}

pub struct ThreadHistoryTables {
    pub history_table: Box<HistoryTable>,
    pub continuation_histories: Box<ContinuationHistoryTables>,
    pub correction_histories: Box<CorrectionHistoryTables>,
}

pub struct ContinuationHistoryTables {
    pub ply1: ContinuationHistoryTable,
    pub ply2: ContinuationHistoryTable,
}

pub static DEFAULT_HISTORY_TABLE: HistoryTable = [[[0; 64]; 6]; 2];

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

        let cont1 = if ply > 1 && Self::can_continuation_history(ss, ply, 2) {
            let zobrist_hash_values = &*ZOBRIST_HASH_VALUES;
            self.cont1
                [(Self::get_continuation_hash(ss, ply, 2, zobrist_hash_values) % CORRECTION_HISTORY_SIZE) as usize]
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

        if ply > 1 && Self::can_continuation_history(ss, ply, 2) {
            let zobrist_hash_values = &*ZOBRIST_HASH_VALUES;
            let cont1 = &mut self.cont1
                [(Self::get_continuation_hash(ss, ply, 2, zobrist_hash_values) % CORRECTION_HISTORY_SIZE) as usize]
                [side];
            *cont1 += (bonus - ((*cont1 as i32) * bonus.abs() / CORRECTION_HISTORY_LIMIT)) as i16;
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
}

impl ContinuationHistoryTables {
    pub fn new() -> Box<Self> {
        let layout = Layout::new::<ContinuationHistoryTables>();

        unsafe {
            // Safety: 0 is a valid value for i16 and ContinuationHistoryTables is a struct of multidimensional arrays of i16
            let mem = alloc_zeroed(layout);

            if mem.is_null() {
                handle_alloc_error(layout);
            }

            let typed_mem = mem.cast::<ContinuationHistoryTables>();
            Box::from_raw(typed_mem)
        }
    }

    #[inline]
    fn get_relevant_histories<'a>(
        &'a self,
        ss: &[SearchStack],
        board: &Board,
        ply: usize,
    ) -> RelevantContinuationHistories<'a> {
        let mut result = [None, None];
        let side = if board.white_to_move { 0 } else { 1 };

        let table_for_ply = &self.ply1;
        let ply_offset = 1;
        if ply >= ply_offset {
            let entry = &ss[ply - ply_offset];
            if entry.get_mov() != EMPTY_MOVE {
                result[0] = Some(
                    &table_for_ply[side][entry.get_moved_piece_type() as usize - 1][entry.get_mov().to() as usize],
                );
            }
        }

        // I think I have to manually repeat this code or run afoul of the rules against mutably borrowing something multiple times
        let table_for_ply = &self.ply2;
        let ply_offset = 2;
        if ply >= ply_offset {
            let entry = &ss[ply - ply_offset];
            if entry.get_mov() != EMPTY_MOVE {
                result[1] = Some(
                    &table_for_ply[side][entry.get_moved_piece_type() as usize - 1][entry.get_mov().to() as usize],
                );
            }
        }

        result
    }

    // TODO: can you combine the mut and non-mut versions?
    #[inline]
    fn get_relevant_histories_mut<'a>(
        &'a mut self,
        ss: &[SearchStack],
        board: &Board,
        ply: usize,
    ) -> MutRelevantContinuationHistories<'a> {
        let mut result = [None, None];
        let side = if board.white_to_move { 0 } else { 1 };

        let table_for_ply = &mut self.ply1;
        let ply_offset = 1;
        if ply >= ply_offset {
            let entry = &ss[ply - ply_offset];
            if entry.get_mov() != EMPTY_MOVE {
                result[0] = Some(
                    &mut table_for_ply[side][entry.get_moved_piece_type() as usize - 1][entry.get_mov().to() as usize],
                );
            }
        }

        // I think I have to manually repeat this code or run afoul of the rules against mutably borrowing something multiple times
        let table_for_ply = &mut self.ply2;
        let ply_offset = 2;
        if ply >= ply_offset {
            let entry = &ss[ply - ply_offset];
            if entry.get_mov() != EMPTY_MOVE {
                result[1] = Some(
                    &mut table_for_ply[side][entry.get_moved_piece_type() as usize - 1][entry.get_mov().to() as usize],
                );
            }
        }

        result
    }

    pub fn apply_to_move_scores(&self, ss: &[SearchStack], board: &Board, moves: &mut [ScoredMove], ply: u8) {
        let relevant_cont_histories = self.get_relevant_histories(ss, board, ply as usize);

        if relevant_cont_histories[0].is_none() && relevant_cont_histories[1].is_none() {
            return;
        }

        for m in moves {
            if m.is_capture() {
                continue;
            }

            let piece_to_move = board.get_piece_64(m.m.from() as usize);
            let piece_index = ((piece_to_move & PIECE_MASK) - 1) as usize;
            let to = m.m.to() as usize;

            if let Some(relevant_cont_hist) = &relevant_cont_histories[0] {
                m.score += relevant_cont_hist[piece_index][to];
            }
            if let Some(relevant_cont_hist) = &relevant_cont_histories[1] {
                m.score += relevant_cont_hist[piece_index][to];
            }
        }
    }
}

impl ThreadHistoryTables {
    pub fn new() -> Self {
        Self {
            history_table: new_boxed_history_table(),
            continuation_histories: ContinuationHistoryTables::new(),
            correction_histories: CorrectionHistoryTables::new(),
        }
    }
}

fn new_boxed_history_table() -> Box<HistoryTable> {
    let layout = Layout::new::<HistoryTable>();

    unsafe {
        // Safety: 0 is a valid value for i16 and HistoryTable is a struct of multidimensional arrays of i16
        let mem = alloc_zeroed(layout);

        if mem.is_null() {
            handle_alloc_error(layout);
        }

        let typed_mem = mem.cast::<HistoryTable>();
        Box::from_raw(typed_mem)
    }
}

#[inline]
pub fn update_killers_and_histories<'a>(
    board: &Board,
    mov: Move,
    draft: u8,
    ply: u8,
    history_table: &mut HistoryTable,
    continuation_histories: &'a mut ContinuationHistoryTables,
    ss: &mut [SearchStack],
    root_killers: &mut [Move; 2],
) -> MutRelevantContinuationHistories<'a> {
    let mut relevant_cont_histories = continuation_histories.get_relevant_histories_mut(ss, board, ply as usize);

    // TODO: Change this to check for capture flag specifically
    if mov.flags() != 0 {
        return relevant_cont_histories;
    }

    let killers = if ply > 0 {
        &mut ss[ply as usize - 1].killers
    } else {
        root_killers
    };

    update_histories(
        board,
        history_table,
        mov,
        (draft as i16) * (draft as i16),
        &mut relevant_cont_histories,
    );

    if killers[0] != mov {
        if killers[1] == mov {
            (killers[0], killers[1]) = (killers[1], killers[0]);
        } else {
            killers[1] = mov;
        }
    }

    relevant_cont_histories
}

#[inline]
pub fn update_histories(
    board: &Board,
    history_table: &mut HistoryTable,
    mov: Move,
    bonus: i16,
    relevant_cont_histories: &mut MutRelevantContinuationHistories,
) {
    // from https://www.chessprogramming.org/History_Heuristic
    let piece_type_index = (board.get_piece_64(mov.from() as usize) & PIECE_MASK) as usize - 1;
    let history_color_value = if board.white_to_move { 0 } else { 1 };
    let to = mov.to() as usize;

    let clamped_history_bonus = (bonus as i32).clamp(-MOVE_SCORE_HISTORY_MAX, MOVE_SCORE_HISTORY_MAX);
    let current_history = &mut history_table[history_color_value][piece_type_index][to];
    *current_history += (clamped_history_bonus
        - ((*current_history as i32) * clamped_history_bonus.abs() / MOVE_SCORE_HISTORY_MAX))
        as i16;

    if let Some(relevant_cont_hist) = &mut relevant_cont_histories[0] {
        let clamped_const_history_bonus =
            (bonus as i32).clamp(-MOVE_SCORE_CONT_HISTORY_PLY1_MAX, MOVE_SCORE_CONT_HISTORY_PLY1_MAX);
        let current_cont_hist = &mut relevant_cont_hist[piece_type_index][to];
        *current_cont_hist += (clamped_const_history_bonus
            - ((*current_cont_hist as i32) * clamped_const_history_bonus.abs() / MOVE_SCORE_CONT_HISTORY_PLY1_MAX))
            as i16;
    }

    if let Some(relevant_cont_hist) = &mut relevant_cont_histories[1] {
        let clamped_const_history_bonus =
            ((bonus / 2) as i32).clamp(-MOVE_SCORE_CONT_HISTORY_PLY2_MAX, MOVE_SCORE_CONT_HISTORY_PLY2_MAX);
        let current_cont_hist = &mut relevant_cont_hist[piece_type_index][to];
        *current_cont_hist += (clamped_const_history_bonus
            - ((*current_cont_hist as i32) * clamped_const_history_bonus.abs() / MOVE_SCORE_CONT_HISTORY_PLY2_MAX))
            as i16;
    }
}

pub fn extra_quiet_move_scoring(
    moves: &mut [ScoredMove],
    board: &Board,
    ss: Option<&[SearchStack]>,
    continuation_histories: Option<&ContinuationHistoryTables>,
    killers: Option<&[Move; 2]>,
    ply: Option<u8>,
) {
    if let Some(ss) = ss
        && let Some(continuation_histories) = continuation_histories
        && let Some(ply) = ply
    {
        continuation_histories.apply_to_move_scores(ss, board, moves, ply);
    }

    if let Some(killers) = killers
        && killers[0] != EMPTY_MOVE
    {
        let mut unmatched_killers = if killers[1] != EMPTY_MOVE { 2 } else { 1 };

        for m in moves {
            if m.m == killers[0] {
                m.score = MOVE_SCORE_KILLER_1;

                if unmatched_killers == 1 {
                    break;
                } else {
                    unmatched_killers -= 1;
                }
            } else if m.m == killers[1] {
                m.score = MOVE_SCORE_KILLER_2;

                if unmatched_killers == 1 {
                    break;
                } else {
                    unmatched_killers -= 1;
                }
            }
        }
    }
}
