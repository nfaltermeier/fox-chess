use std::{
    io,
    sync::mpsc::{self, Receiver, TryRecvError},
    thread::{self, sleep},
    time::{Duration, Instant, SystemTime},
};

use board::Board;
use log::{debug, error, info, warn};
use move_generator::{
    can_capture_opponent_king, generate_moves, generate_moves_psuedo_legal, perft, perft_pseudo_legal_optimized,
    PerftStats,
};
use moves::{square_indices_to_moves, Move, MoveRollback};
use search::prioritize_moves;
use uci::UciInterface;

use num_format::{Locale, ToFormattedString};

mod board;
mod evaluate;
mod move_generator;
mod moves;
mod search;
mod uci;

pub static STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

fn main() {
    let setup_logger_result = setup_logger();
    if setup_logger_result.is_err() {
        panic!("logger setup failed: {}", setup_logger_result.unwrap_err())
    }
    log_panics::init();

    run_uci();

    // print_moves_from_pos("4kb1r/r5pp/8/4p2n/1pP5/8/PP4PP/4KRNR w Kk - 0 1");
    // do_perft(5, STARTING_FEN);
    // do_perft(4, "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // do_perft(6, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 1 1");
    // do_perft(5, "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1");
    // do_perft(4, "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
    // do_perft(4, "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10");
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
        info!(
            "depth {depth} in {elapsed:#?}. Nodes: {}. Nodes per second: {}",
            stats.nodes.to_formatted_string(&Locale::en), (nps as u64).to_formatted_string(&Locale::en)
        );
        info!("{:?}", stats);
        debug_assert!(rollback.is_empty());
    }
}

fn print_moves_from_pos(fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    info!("{:?}", &board);

    let mut moves = generate_moves(&mut board);

    prioritize_moves(&mut moves, &board);

    for r#move in moves {
        info!("{}", r#move.pretty_print(Some(&board)));
    }
}

fn run_to_pos(index_moves: Vec<(u8, u8, Option<u16>)>) {
    let moves = square_indices_to_moves(index_moves);
    let mut board = Board::from_fen(STARTING_FEN).unwrap();
    let mut rollback = MoveRollback::default();
    for r#move in moves {
        board.make_move(&r#move, &mut rollback);
    }

    let final_pos_moves = generate_moves(&mut board);

    if final_pos_moves.is_empty() {
        warn!("No moves found")
    }

    for r#move in final_pos_moves {
        info!("{}", r#move.pretty_print(Some(&board)));
    }
}

fn make_moves(moves: Vec<Move>, fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    let mut rollback = MoveRollback::default();
    for r#move in moves {
        debug!("{:?}", board);
        debug!("{}", r#move.pretty_print(Some(&board)));
        board.make_move(&r#move, &mut rollback);
        let legal = !can_capture_opponent_king(&board, true);
        debug!("legality: {}", legal)
    }

    debug!("After all moves");
    debug!("{:?}", board);

    let final_pos_moves = generate_moves(&mut board);

    if final_pos_moves.is_empty() {
        warn!("No moves found")
    }

    for r#move in final_pos_moves {
        info!("{}", r#move.pretty_print(Some(&board)));
    }
}

fn setup_logger() -> Result<(), fern::InitError> {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{} {} {}] {}",
                humantime::format_rfc3339_seconds(SystemTime::now()),
                record.level(),
                record.target(),
                message
            ))
        })
        .level(log::LevelFilter::Debug)
        // .level_for("fox_chess::move_generator", log::LevelFilter::Trace)
        .chain(std::io::stderr())
        .chain(fern::log_file("output.log")?)
        .apply()?;
    Ok(())
}

fn run_uci() {
    let mut uci = UciInterface::default();
    let stdin_channel = spawn_stdin_channel();
    loop {
        match stdin_channel.try_recv() {
            Ok(val) => {
                uci.process_command(val);
            }
            Err(TryRecvError::Empty) => {}
            Err(TryRecvError::Disconnected) => {
                error!("stdin channel disconnected");
                panic!("stdin channel disconnected")
            }
        }
        sleep(Duration::from_millis(50));
    }
}

// From https://stackoverflow.com/a/55201400
fn spawn_stdin_channel() -> Receiver<String> {
    let (tx, rx) = mpsc::channel::<String>();
    thread::spawn(move || loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).unwrap();
        tx.send(buffer).unwrap();
    });
    rx
}
