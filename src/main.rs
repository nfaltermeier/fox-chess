use std::time::Instant;

use board::Board;
use move_generator::{can_capture_opponent_king, generate_moves, generate_moves_psuedo_legal, perft, perft_pseudo_legal_optimized, PerftStats};
use moves::{square_indices_to_moves, Move, MoveRollback};

mod board;
mod move_generator;
mod moves;

pub static STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

fn main() {
    println!("Hello, world!");

    // print_moves_from_pos("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q2/PPPBBPpP/1R2K2R w Kkq - 0 2");
    // do_perft(5, STARTING_FEN);
    // do_perft(4, "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // do_perft(6, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 1 1");
    // do_perft(5, "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1");
    // do_perft(4, "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
    do_perft(4, "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10");
    // make_moves(Vec::from([ Move { data: 0x0040 }, Move { data: 0x4397 }, Move { data: 0x0144 }, Move { data: 0xc14e } ]), "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // run_to_pos(Vec::from([(12, 28), (52, 36), (3, 39), (57, 42), (5, 26), (62, 45), (39, 53)]));
}

fn do_perft(up_to_depth: u8, fen: &str) {
    for depth in 1..=up_to_depth {
        let mut board = Board::from_fen(fen).unwrap();
        let mut rollback = MoveRollback::default();
        let mut stats = PerftStats::default();

        let start_time = Instant::now();
        perft(depth, &mut board, &mut rollback, &mut stats);
        let elapsed = start_time.elapsed();

        let nps = stats.nodes as f64 / elapsed.as_secs_f64();
        println!("depth {depth} in {elapsed:#?}. Nodes: {}. Nodes per second: {nps:.0}", stats.nodes);
        dbg!(stats);
        debug_assert!(rollback.is_empty());
    }
}

fn print_moves_from_pos(fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    dbg!(&board);

    let moves = generate_moves(&mut board);
    for r#move in moves {
        println!("{}", r#move.pretty_print(Some(&board)));
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
        println!("{}", r#move.pretty_print(Some(&board)));
    }
}

fn make_moves(moves: Vec<Move>, fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    let mut rollback = MoveRollback::default();
    for r#move in moves {
        dbg!(board);
        println!("{}", r#move.pretty_print(Some(&board)));
        board.make_move(&r#move, &mut rollback);
        let legal = !can_capture_opponent_king(&board, true);
        println!("legality: {}", legal)
    }

    println!("After all moves");
    dbg!(board);

    let final_pos_moves = generate_moves(&mut board);

    if final_pos_moves.is_empty() {
        println!("No moves found")
    }

    for r#move in final_pos_moves {
        println!("{}", r#move.pretty_print(Some(&board)));
    }
}
