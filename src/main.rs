use board::Board;
use move_generator::generate_moves_psuedo_legal;

mod board;
mod move_generator;
mod moves;

fn main() {
    println!("Hello, world!");
    let board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
    dbg!(&board);

    let moves = generate_moves_psuedo_legal(&board);
    println!("{}", moves.len())
}
