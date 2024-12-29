use std::{
    cmp::Ordering,
    time::{Duration, Instant},
};

use log::error;
use vampirc_uci::{UciSearchControl, UciTimeControl};

use crate::{
    board::{Board, PIECE_MASK},
    evaluate::CENTIPAWN_VALUES,
    move_generator::{can_capture_opponent_king, generate_moves},
    moves::{Move, MoveRollback, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_CAPTURE_FULL},
    uci::UciInterface,
};

#[derive(Default)]
pub struct SearchStats {
    pub nodes: u64,
    pub depth: u8,
}

impl Board {
    pub fn search(&mut self, time: &Option<UciTimeControl>) -> (Move, Option<i32>, SearchStats) {
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

        // (self.random_move(), 0)
        // self.negamax_init(4)
        let start_time = Instant::now();
        let result = self.alpha_beta_init(draft).0;
        let elapsed = start_time.elapsed();

        UciInterface::print_search_info(result.1, &result.2, &elapsed);

        result
    }

    pub fn iterative_deepening_search(
        &mut self,
        time: &Option<UciTimeControl>,
        search: &Option<UciSearchControl>,
    ) -> (Move, Option<i32>, SearchStats) {
        let start_time = Instant::now();
        let target_dur;

        if let Some(t) = time {
            match t {
                UciTimeControl::TimeLeft { white_time, black_time, white_increment, black_increment, moves_to_go } => {
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
                    target_dur = time_left.as_ref().unwrap().to_std().unwrap().checked_div(divisor).unwrap();
                    // let expected_moves_left = if let Some(mtg) = moves_to_go {
                    //     *mtg
                    // } else {
                    //     let eval = self.evaluate();
                    // }
                },
                UciTimeControl::MoveTime(time_delta) => {
                    target_dur = time_delta.to_std().unwrap();
                },
                UciTimeControl::Ponder => {
                    unimplemented!("uci go ponder");
                },
                UciTimeControl::Infinite => {
                    unimplemented!("uci go infinite");
                },
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
            let (result, end_search) = self.alpha_beta_init(depth);
            let elapsed = start_time.elapsed();

            UciInterface::print_search_info(result.1, &result.2, &elapsed);

            if end_search || (depth >= 5 && elapsed >= cutoff) || elapsed >= cutoff_low_depth || result.1.is_some_and(|v| v.abs() >= 19800) {
                return result;
            }

            depth += 1;
        }
    }

    pub fn alpha_beta_init(&mut self, draft: u8) -> ((Move, Option<i32>, SearchStats), bool) {
        let mut alpha = -999999;
        let mut best_value = -999999;
        let mut best_move = None;
        let mut moves = generate_moves(self);
        let mut rollback = MoveRollback::default();
        let mut stats = SearchStats::default();

        if moves.is_empty() {
            error!(
                "Tried to search on a position but found no moves. Position: {:#?}",
                self
            );
            panic!("Tried to search on a position but found no moves");
        }

        if moves.len() == 1 {
            stats.depth = 1;
            return ((moves.pop().unwrap(), None, stats), true);
        } else {
            stats.depth = draft;
        }

        prioritize_moves(&mut moves, self);

        for r#move in moves {
            let repetitions = self.make_move(&r#move, &mut rollback);

            let result;
            if repetitions >= 3 || self.halfmove_clock >= 50 {
                result = 0;
            } else {
                result = -self.alpha_beta_recurse(-999999, -alpha, draft - 1, 1, &mut rollback, &mut stats);
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
                Some(best_value * if self.white_to_move { 1 } else { -1 }),
                stats,
            ),
            false,
        )
    }

    fn alpha_beta_recurse(
        &mut self,
        mut alpha: i32,
        beta: i32,
        draft: u8,
        ply: u8,
        rollback: &mut MoveRollback,
        stats: &mut SearchStats,
    ) -> i32 {
        if draft == 0 {
            return self.quiescense_side_to_move_relative(alpha, beta, ply + 1, rollback, stats);
        }

        let mut best_value = -999999;
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
            let repetitions = self.make_move(&r#move, rollback);

            let result;
            if repetitions >= 3 || self.halfmove_clock >= 50 {
                result = 0;
            } else {
                result = -self.alpha_beta_recurse(-beta, -alpha, draft - 1, ply + 1, rollback, stats);
            }

            self.unmake_move(&r#move, rollback);

            if result > best_value {
                best_value = result;
                if result > alpha {
                    alpha = result;
                }
            }

            if result >= beta {
                return best_value;
            }
        }

        best_value
    }

    pub fn quiescense_side_to_move_relative(
        &mut self,
        mut alpha: i32,
        beta: i32,
        ply: u8,
        rollback: &mut MoveRollback,
        stats: &mut SearchStats,
    ) -> i32 {
        let stand_pat = self.evaluate_side_to_move_relative();
        stats.nodes += 1;

        if stand_pat >= beta {
            return beta;
        }
        if alpha < stand_pat {
            alpha = stand_pat;
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
                result = -self.quiescense_side_to_move_relative(-beta, -alpha, ply + 1, rollback, stats);
            }

            self.unmake_move(&r#move, rollback);

            if result >= beta {
                return beta;
            }
            if alpha < result {
                alpha = result;
            }
        }

        alpha
    }

    pub fn random_move(&mut self) -> Move {
        let mut moves = generate_moves(self);

        if moves.is_empty() {
            error!("Search generated no moves for the current position");
            panic!("Search generated no moves for the current position");
        }

        moves.swap_remove(rand::random::<usize>() % moves.len())
    }

    pub fn negamax_init(&mut self, draft: u8) -> (Move, i32) {
        let mut max = -999999;
        let mut max_move = None;
        let moves = generate_moves(self);
        let mut rollback = MoveRollback::default();

        if moves.is_empty() {
            error!(
                "Tried to search on a position but found no moves. Position: {:#?}",
                self
            );
            panic!("Tried to search on a position but found no moves");
        }

        for r#move in moves {
            self.make_move(&r#move, &mut rollback);
            let result = -self.negamax_recurse(draft - 1, 1);
            self.unmake_move(&r#move, &mut rollback);

            if result > max {
                max = result;
                max_move = Some(r#move);
            }
        }

        // Make the score not side-to-move relative
        (max_move.unwrap(), max * if self.white_to_move { 1 } else { -1 })
    }

    fn negamax_recurse(&mut self, draft: u8, ply: u8) -> i32 {
        if draft == 0 {
            return self.evaluate_side_to_move_relative();
        }

        let mut max = -999999;
        let moves = generate_moves(self);
        let mut rollback = MoveRollback::default();

        // Assuming no bug with move generation...
        if moves.is_empty() {
            self.white_to_move = !self.white_to_move;
            let is_check = can_capture_opponent_king(self, false);
            self.white_to_move = !self.white_to_move;

            return if is_check {
                self.evaluate_checkmate_side_to_move_relative(ply)
            } else {
                0
            };
        }

        for r#move in moves {
            self.make_move(&r#move, &mut rollback);
            let result = -self.negamax_recurse(draft - 1, ply + 1);
            self.unmake_move(&r#move, &mut rollback);

            if result > max {
                max = result;
            }
        }

        max
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
