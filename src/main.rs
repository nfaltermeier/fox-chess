use std::time::Instant;

use board::Board;
use move_generator::{generate_moves, generate_moves_psuedo_legal, perft, perft_pseudo_legal_optimized, PerftStats};
use moves::{square_indices_to_moves, MoveRollback};

mod board;
mod move_generator;
mod moves;

pub static STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

fn main() {
    println!("Hello, world!");

    // print_moves_from_pos("r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4");
    do_perft(4);
    // run_to_pos(Vec::from([(12, 28), (52, 36), (3, 39), (57, 42), (5, 26), (62, 45), (39, 53)]));
}

fn do_perft(up_to_depth: u8) {
    for depth in 1..=up_to_depth {
        let mut board = Board::from_fen(STARTING_FEN).unwrap();
        let mut rollback = MoveRollback::default();
        let mut stats = PerftStats::default();

        let start_time = Instant::now();
        perft(depth, &mut board, &mut rollback, &mut stats);
        let elapsed = start_time.elapsed();

        println!("depth {depth} in {elapsed:#?}. Nodes: {}", stats.nodes);
        dbg!(stats);
        debug_assert!(rollback.is_empty());
    }
}

fn print_moves_from_pos(fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    dbg!(&board);

    let moves = generate_moves(&mut board);
    for r#move in moves {
        println!("{}", r#move.pretty_print(&board));
    }
}

fn run_to_pos(index_moves: Vec<(u8, u8)>) {
    let moves = square_indices_to_moves(index_moves);
    let mut board = Board::from_fen(STARTING_FEN).unwrap();
    let mut rollback = MoveRollback::default();
    for r#move in moves {
        board.make_move(&r#move, &mut rollback);
    }

    let final_pos_moves = generate_moves(&mut board);

    if final_pos_moves.is_empty() {
        println!("No moves found")
    }

    for r#move in final_pos_moves {
        println!("{}", r#move.pretty_print(&board));
    }
}
