use board::Board;

mod board;

fn main() {
    println!("Hello, world!");
    let board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    dbg!(&board);
}
