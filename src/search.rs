use log::error;

use crate::{board::Board, move_generator::generate_moves, moves::Move};

impl Board {
    pub fn search(&mut self) -> Move {
        let mut moves = generate_moves(self);

        if moves.is_empty() {
            error!("Search generated no moves for the current position");
            panic!("Search generated no moves for the current position");
        }

        moves.swap_remove(rand::random::<usize>() % moves.len())
    }
}
