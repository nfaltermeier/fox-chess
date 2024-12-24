use log::error;

use crate::{board::Board, move_generator::generate_moves, moves::{Move, MoveRollback}};

impl Board {
    pub fn search(&mut self) -> (Move, i32) {
        // (self.random_move(), 0)
        self.negamax_init(4)
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
        let mut max = -9999;
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

        (max_move.unwrap(), max)
    }

    fn negamax_recurse(&mut self, depth: u8) -> i32 {
        if depth == 0 {
            return self.evaluate();
        }

        let mut max = -9999;
        let moves = generate_moves(self);
        let mut rollback = MoveRollback::default();

        // This should mean checkmate...
        if moves.is_empty() {
            return self.evaluate_checkmate()
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
