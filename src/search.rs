use std::cmp::Ordering;

use log::error;

use crate::{board::Board, move_generator::{can_capture_opponent_king, generate_moves}, moves::{Move, MoveRollback, MOVE_FLAG_CAPTURE, MOVE_FLAG_CAPTURE_FULL}};

#[derive(Default)]
pub struct SearchStats {
    pub nodes: u64,
    pub depth: u8,
}

impl Board {
    pub fn search(&mut self) -> (Move, i32, SearchStats) {
        // (self.random_move(), 0)
        // self.negamax_init(4)
        self.alpha_beta_init(4)
    }

    pub fn alpha_beta_init(&mut self, depth: u8) -> (Move, i32, SearchStats) {
        let mut alpha = -999999;
        let mut best_value = -999999;
        let mut best_move = None;
        let moves = generate_moves(self);
        let mut rollback = MoveRollback::default();
        let mut stats = SearchStats::default();
        stats.depth = depth;

        if moves.is_empty() {
            error!("Tried to search on a position but found no moves. Position: {:#?}", self);
            panic!("Tried to search on a position but found no moves");
        }

        for r#move in moves {
            self.make_move(&r#move, &mut rollback);
            let result = -self.alpha_beta_recurse(-999999, -alpha, depth - 1, &mut rollback, &mut stats);
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
        (best_move.unwrap(), best_value * if self.white_to_move { 1 } else { -1 }, stats)
    }

    fn alpha_beta_recurse(&mut self, mut alpha: i32, beta: i32, depth: u8, rollback: &mut MoveRollback, stats: &mut SearchStats) -> i32 {
        if depth == 0 {
            return self.quiescense_side_to_move_relative(alpha, beta, rollback, stats);
        }

        let mut best_value = -999999;
        let moves = generate_moves(self);

        // Assuming no bug with move generation...
        if moves.is_empty() {
            self.white_to_move = !self.white_to_move;
            let is_check = can_capture_opponent_king(self, false);
            self.white_to_move = !self.white_to_move;

            return if is_check { self.evaluate_checkmate_side_to_move_relative() } else { 0 }
        }

        for r#move in moves {
            self.make_move(&r#move, rollback);
            let result = -self.alpha_beta_recurse(-beta, -alpha, depth - 1, rollback, stats);
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

    pub fn quiescense_side_to_move_relative(&mut self, mut alpha: i32, beta: i32, rollback: &mut MoveRollback, stats: &mut SearchStats) -> i32 {
        let stand_pat = self.evaluate_side_to_move_relative();
        stats.nodes += 1;

        if stand_pat >= beta {
            return beta;
        }
        if alpha < stand_pat {
            alpha = stand_pat;
        }

        let moves = generate_moves(self);
        for r#move in moves.iter().filter(|m| m.flags() & MOVE_FLAG_CAPTURE != 0) {
            self.make_move(&r#move, rollback);
            let result = -self.quiescense_side_to_move_relative(-beta, -alpha, rollback, stats);
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

    pub fn negamax_init(&mut self, depth: u8) -> (Move, i32) {
        let mut max = -999999;
        let mut max_move = None;
        let moves = generate_moves(self);
        let mut rollback = MoveRollback::default();

        if moves.is_empty() {
            error!("Tried to search on a position but found no moves. Position: {:#?}", self);
            panic!("Tried to search on a position but found no moves");
        }

        for r#move in moves {
            self.make_move(&r#move, &mut rollback);
            let result = -self.negamax_recurse(depth - 1);
            self.unmake_move(&r#move, &mut rollback);

            if result > max {
                max = result;
                max_move = Some(r#move);
            }
        }

        // Make the score not side-to-move relative
        (max_move.unwrap(), max * if self.white_to_move { 1 } else { -1 })
    }

    fn negamax_recurse(&mut self, depth: u8) -> i32 {
        if depth == 0 {
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

            return if is_check { self.evaluate_checkmate_side_to_move_relative() } else { 0 }
        }

        for r#move in moves {
            self.make_move(&r#move, &mut rollback);
            let result = -self.negamax_recurse(depth - 1);
            self.unmake_move(&r#move, &mut rollback);

            if result > max {
                max = result;
            }
        }

        max
    }
}
