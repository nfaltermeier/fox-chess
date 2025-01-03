use std::{
    cmp::Ordering,
    i16,
    time::{Duration, Instant},
};

use log::error;
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_MASK},
    evaluate::{CENTIPAWN_VALUES, ENDGAME_GAME_STAGE_FOR_QUIESCENSE},
    move_generator::{can_capture_opponent_king, generate_moves},
    moves::{Move, MoveRollback, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_CAPTURE_FULL},
    transposition_table::{self, MoveType, TTEntry, TableType, TranspositionTable},
    uci::UciInterface,
};

// pub const TEST_TT_FOR_HASH_COLLISION: bool = true;

#[derive(Default)]
pub struct SearchStats {
    pub quiescense_nodes: u64,
    pub depth: u8,
    pub quiescense_cut_by_hopeless: u64,
    pub leaf_nodes: u64,
}

impl Board {
    pub fn search(
        &mut self,
        time: &Option<UciTimeControl>,
        transposition_table: &mut TranspositionTable,
    ) -> (Move, i16, SearchStats) {
        let mut draft;
        if cfg!(debug_assertions) {
            draft = 4;
        } else {
            draft = 5;
        }

        if time.is_some() {
            let t = time.as_ref().unwrap();
            match t {
                UciTimeControl::TimeLeft {
                    white_time,
                    black_time,
                    white_increment,
                    black_increment,
                    moves_to_go,
                } => {
                    if self.white_to_move {
                        if white_time.is_some() && white_time.as_ref().unwrap().num_seconds() < 30 {
                            draft -= 1;
                        }
                    } else if black_time.is_some() && black_time.as_ref().unwrap().num_seconds() < 30 {
                        draft -= 1;
                    }
                }
                UciTimeControl::MoveTime(dur) => {
                    if dur.num_seconds() < 5 {
                        draft -= 1;
                    }
                }
                _ => {}
            }
        }

        let start_time = Instant::now();
        let result = self.alpha_beta_init(draft, transposition_table).0;
        let elapsed = start_time.elapsed();

        UciInterface::print_search_info(result.1, &result.2, &elapsed);

        result
    }

    pub fn iterative_deepening_search(
        &mut self,
        time: &Option<UciTimeControl>,
        search: &Option<UciSearchControl>,
        transposition_table: &mut TranspositionTable,
    ) -> (Move, i16, SearchStats) {
        let start_time = Instant::now();
        let target_dur;

        if let Some(t) = time {
            match t {
                UciTimeControl::TimeLeft {
                    white_time,
                    black_time,
                    white_increment,
                    black_increment,
                    moves_to_go,
                } => {
                    let time_left = if self.white_to_move { white_time } else { black_time };

                    if time_left.is_none() {
                        error!("No time left value provided when searching");
                        panic!("No time left value provided when searching");
                    }

                    let divisor = if self.fullmove_counter < 10 {
                        21
                    } else if self.fullmove_counter < 20 {
                        18
                    } else {
                        25
                    };
                    target_dur = time_left
                        .as_ref()
                        .unwrap()
                        .to_std()
                        .unwrap()
                        .checked_div(divisor)
                        .unwrap();

                    // maybe something like https://www.desmos.com/calculator/47t9iys2fo
                    // let expected_moves_left = if let Some(mtg) = moves_to_go {
                    //     *mtg
                    // } else {
                    //     let eval = self.evaluate();
                    // }
                }
                UciTimeControl::MoveTime(time_delta) => {
                    target_dur = time_delta.to_std().unwrap();
                }
                UciTimeControl::Ponder => {
                    unimplemented!("uci go ponder");
                }
                UciTimeControl::Infinite => {
                    unimplemented!("uci go infinite");
                }
            }
        } else {
            error!("UCI time control not passed to go, currently required");
            unimplemented!("UCI time control not passed to go, currently required");
        }

        // Above 5 depth moves can start taking a lot more time
        let cutoff_low_depth = target_dur.mul_f32(0.55);
        let cutoff = target_dur.mul_f32(0.25);
        let mut depth = 1;

        loop {
            let (result, end_search) = self.alpha_beta_init(depth, transposition_table);
            let elapsed = start_time.elapsed();

            // print less when using TT values
            if result.2.leaf_nodes != 0 || depth == 1 {
                UciInterface::print_search_info(result.1, &result.2, &elapsed);
            }

            if end_search
                || (depth >= 5 && elapsed >= cutoff)
                || elapsed >= cutoff_low_depth
                || result.1.abs() >= 19800
                || depth >= 40
            {
                return result;
            }

            depth += 1;
        }
    }

    pub fn alpha_beta_init(
        &mut self,
        draft: u8,
        transposition_table: &mut TranspositionTable,
    ) -> ((Move, i16, SearchStats), bool) {
        let mut alpha = -i16::MAX;
        let mut best_value = -i16::MAX;
        let mut best_move = None;
        let mut rollback = MoveRollback::default();
        let mut stats = SearchStats::default();

        let tt_entry = transposition_table.get_entry(self.hash, TableType::Main);
        if let Some(tt_data) = tt_entry {
            if tt_data.move_num >= self.fullmove_counter + draft as u16 {
                match tt_data.move_type {
                    transposition_table::MoveType::Best => {
                        // should this be done for the root????
                        return (
                            (
                                tt_data.important_move,
                                tt_data.eval * if self.white_to_move { 1 } else { -1 },
                                stats,
                            ),
                            false,
                        );
                    }
                    _ => {}
                }
            }

            let repetitions = self.make_move(&tt_data.important_move, &mut rollback);

            let result;
            if repetitions >= 3 || self.halfmove_clock >= 50 {
                result = 0;
            } else {
                result = -self.alpha_beta_recurse(
                    -i16::MAX,
                    -alpha,
                    draft - 1,
                    1,
                    &mut rollback,
                    &mut stats,
                    transposition_table,
                );
            }

            self.unmake_move(&tt_data.important_move, &mut rollback);

            if result > best_value {
                best_value = result;
                best_move = Some(tt_data.important_move);
                if result > alpha {
                    alpha = result;
                }
            }
        }

        let mut moves = generate_moves(self);

        if moves.is_empty() {
            error!(
                "Tried to search on a position but found no moves. Position: {:#?}",
                self
            );
            panic!("Tried to search on a position but found no moves");
        }

        if moves.len() == 1 {
            stats.depth = 1;
            return ((moves.pop().unwrap(), self.evaluate(), stats), true);
        } else {
            stats.depth = draft;
        }

        prioritize_moves(&mut moves, self);

        for r#move in moves {
            if tt_entry.is_some_and(|v| v.important_move == r#move) {
                continue;
            }

            let repetitions = self.make_move(&r#move, &mut rollback);

            let result;
            if repetitions >= 3 || self.halfmove_clock >= 50 {
                result = 0;
            } else {
                result = -self.alpha_beta_recurse(
                    -i16::MAX,
                    -alpha,
                    draft - 1,
                    1,
                    &mut rollback,
                    &mut stats,
                    transposition_table,
                );
            }

            self.unmake_move(&r#move, &mut rollback);

            if result > best_value {
                best_value = result;
                best_move = Some(r#move);
                if result > alpha {
                    alpha = result;
                }
            }
        }

        // Make the score not side-to-move relative
        (
            (
                best_move.unwrap(),
                best_value * if self.white_to_move { 1 } else { -1 },
                stats,
            ),
            false,
        )
    }

    fn alpha_beta_recurse(
        &mut self,
        mut alpha: i16,
        beta: i16,
        draft: u8,
        ply: u8,
        rollback: &mut MoveRollback,
        stats: &mut SearchStats,
        transposition_table: &mut TranspositionTable,
    ) -> i16 {
        if draft == 0 {
            stats.leaf_nodes += 1;
            return self.quiescense_side_to_move_relative(alpha, beta, ply + 1, rollback, stats, transposition_table);
        }

        let mut best_value = -i16::MAX;
        let mut best_move = None;

        let tt_entry = transposition_table.get_entry(self.hash, TableType::Main);
        if let Some(tt_data) = tt_entry {
            if tt_data.move_num >= self.fullmove_counter + draft as u16 {
                match tt_data.move_type {
                    transposition_table::MoveType::FailHigh => {
                        if tt_data.eval >= beta {
                            return tt_data.eval;
                        }
                    }
                    transposition_table::MoveType::Best => {
                        return tt_data.eval;
                    }
                    transposition_table::MoveType::FailLow => {
                        if tt_data.eval < alpha {
                            return tt_data.eval;
                        }
                    }
                }
            }

            let repetitions = self.make_move(&tt_data.important_move, rollback);

            let result;
            if repetitions >= 3 || self.halfmove_clock >= 50 {
                result = 0;
            } else {
                result =
                    -self.alpha_beta_recurse(-beta, -alpha, draft - 1, ply + 1, rollback, stats, transposition_table);
            }

            self.unmake_move(&tt_data.important_move, rollback);

            if result >= beta {
                transposition_table.store_entry(
                    TTEntry {
                        hash: self.hash,
                        important_move: tt_data.important_move,
                        move_type: MoveType::FailHigh,
                        eval: result,
                        move_num: self.fullmove_counter + draft as u16,
                    },
                    TableType::Main,
                );

                return result;
            }

            if result > best_value {
                best_value = result;
                best_move = Some(tt_data.important_move);
                if result > alpha {
                    alpha = result;
                }
            }
        }

        let mut moves = generate_moves(self);

        // Assuming no bug with move generation...
        if moves.is_empty() {
            self.white_to_move = !self.white_to_move;
            let is_check = can_capture_opponent_king(self, false);
            self.white_to_move = !self.white_to_move;

            if is_check {
                return self.evaluate_checkmate_side_to_move_relative(ply);
            } else {
                return 0;
            }
        }

        prioritize_moves(&mut moves, self);

        for r#move in moves {
            if tt_entry.is_some_and(|v| v.important_move == r#move) {
                continue;
            }

            let repetitions = self.make_move(&r#move, rollback);

            let result;
            if repetitions >= 3 || self.halfmove_clock >= 50 {
                result = 0;
            } else {
                result =
                    -self.alpha_beta_recurse(-beta, -alpha, draft - 1, ply + 1, rollback, stats, transposition_table);
            }

            self.unmake_move(&r#move, rollback);

            if result >= beta {
                transposition_table.store_entry(
                    TTEntry {
                        hash: self.hash,
                        important_move: r#move,
                        move_type: MoveType::FailHigh,
                        eval: result,
                        move_num: self.fullmove_counter + draft as u16,
                    },
                    TableType::Main,
                );

                return result;
            }

            if result > best_value {
                best_value = result;
                best_move = Some(r#move);
                if result > alpha {
                    alpha = result;
                }
            }
        }

        transposition_table.store_entry(
            TTEntry {
                hash: self.hash,
                important_move: best_move.unwrap(),
                move_type: if alpha == best_value {
                    MoveType::Best
                } else {
                    MoveType::FailLow
                },
                eval: best_value,
                move_num: self.fullmove_counter + draft as u16,
            },
            TableType::Main,
        );

        best_value
    }

    pub fn quiescense_side_to_move_relative(
        &mut self,
        mut alpha: i16,
        beta: i16,
        ply: u8,
        rollback: &mut MoveRollback,
        stats: &mut SearchStats,
        transposition_table: &mut TranspositionTable,
    ) -> i16 {
        stats.quiescense_nodes += 1;

        let tt_entry = transposition_table.get_entry(self.hash, TableType::Quiescense);
        if let Some(tt_data) = tt_entry {
            if tt_data.move_num >= self.fullmove_counter {
                match tt_data.move_type {
                    transposition_table::MoveType::FailHigh => {
                        if tt_data.eval >= beta {
                            return tt_data.eval;
                        }
                    }
                    transposition_table::MoveType::Best => {
                        return tt_data.eval;
                    }
                    transposition_table::MoveType::FailLow => {
                        if tt_data.eval < alpha {
                            return tt_data.eval;
                        }
                    }
                }
            }
        }

        let stand_pat = self.evaluate_side_to_move_relative();

        if stand_pat >= beta {
            return stand_pat;
        }

        if self.game_stage > ENDGAME_GAME_STAGE_FOR_QUIESCENSE {
            // avoid underflow
            if alpha >= i16::MIN + 1000 && stand_pat < alpha - 1000 {
                stats.quiescense_cut_by_hopeless += 1;
                return alpha;
            }
        }

        if alpha < stand_pat {
            alpha = stand_pat;
        }

        let mut best_value = stand_pat;
        let mut best_move = None;

        if let Some(tt_data) = tt_entry {
            if tt_data.important_move.flags() & MOVE_FLAG_CAPTURE != 0 {
                let repetitions = self.make_move(&tt_data.important_move, rollback);

                let result;
                if repetitions >= 3 || self.halfmove_clock >= 50 {
                    result = 0;
                } else {
                    result = -self.quiescense_side_to_move_relative(
                        -beta,
                        -alpha,
                        ply + 1,
                        rollback,
                        stats,
                        transposition_table,
                    );
                }

                self.unmake_move(&tt_data.important_move, rollback);

                if result >= beta {
                    transposition_table.store_entry(
                        TTEntry {
                            hash: self.hash,
                            important_move: tt_data.important_move,
                            move_type: MoveType::FailHigh,
                            eval: result,
                            move_num: self.fullmove_counter,
                        },
                        TableType::Quiescense,
                    );

                    return result;
                }

                if result > best_value {
                    best_value = result;
                    best_move = Some(tt_data.important_move);
                    if result > alpha {
                        alpha = result;
                    }
                }
            }
        }

        let moves = generate_moves(self);

        if moves.is_empty() {
            self.white_to_move = !self.white_to_move;
            let is_check = can_capture_opponent_king(self, false);
            self.white_to_move = !self.white_to_move;

            if is_check {
                return self.evaluate_checkmate_side_to_move_relative(ply);
            } else {
                return 0;
            }
        }

        let mut capture_moves = moves
            .into_iter()
            .filter(|m| m.flags() & MOVE_FLAG_CAPTURE != 0)
            .collect::<Vec<Move>>();

        prioritize_moves(&mut capture_moves, self);

        for r#move in capture_moves {
            let repetitions = self.make_move(&r#move, rollback);
            // pretty sure checkmate and repetition checks are needed here or in this method somewhere
            let result;

            // Only doing captures right now so not checking halfmove here
            if repetitions >= 3 {
                result = 0;
            } else {
                result = -self.quiescense_side_to_move_relative(
                    -beta,
                    -alpha,
                    ply + 1,
                    rollback,
                    stats,
                    transposition_table,
                );
            }

            self.unmake_move(&r#move, rollback);

            if result >= beta {
                return result;
            }

            if best_value < result {
                best_value = result;
                best_move = Some(r#move);

                if alpha < result {
                    alpha = result;
                }
            }
        }

        if let Some(bm) = best_move {
            transposition_table.store_entry(
                TTEntry {
                    hash: self.hash,
                    important_move: bm,
                    move_type: if alpha == best_value {
                        MoveType::Best
                    } else {
                        MoveType::FailLow
                    },
                    eval: best_value,
                    move_num: self.fullmove_counter,
                },
                TableType::Quiescense,
            );
        }

        best_value
    }
}

pub fn prioritize_moves(moves: &mut Vec<Move>, board: &Board) {
    moves.sort_by(|m1, m2| {
        // Should promotions be given a bonus for capture or non-capture?
        let m1_capture = m1.data & MOVE_FLAG_CAPTURE_FULL != 0;
        let m2_capture = m2.data & MOVE_FLAG_CAPTURE_FULL != 0;
        let capture_cmp = m2_capture.cmp(&m1_capture);

        if !m1_capture || capture_cmp != Ordering::Equal {
            return capture_cmp;
        }

        let m1_cp_diff;
        let m2_cp_diff;

        if m1.flags() == MOVE_EP_CAPTURE {
            m1_cp_diff = 0;
        } else {
            let p1_from = board.get_piece_64(m1.from() as usize);
            let p1_to = board.get_piece_64(m1.to() as usize);
            m1_cp_diff =
                CENTIPAWN_VALUES[(p1_to & PIECE_MASK) as usize] - CENTIPAWN_VALUES[(p1_from & PIECE_MASK) as usize];
        }

        if m2.flags() == MOVE_EP_CAPTURE {
            m2_cp_diff = 0;
        } else {
            let p2_from = board.get_piece_64(m2.from() as usize);
            let p2_to = board.get_piece_64(m2.to() as usize);
            m2_cp_diff =
                CENTIPAWN_VALUES[(p2_to & PIECE_MASK) as usize] - CENTIPAWN_VALUES[(p2_from & PIECE_MASK) as usize];
        }

        // trace!("{} {} {} {} {:?}", m1.pretty_print(Some(board)), m1_cp_diff, m2.pretty_print(Some(board)), m2_cp_diff, result);
        m2_cp_diff.cmp(&m1_cp_diff)
    });
}
