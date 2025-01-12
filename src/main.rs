use std::{
    cmp::Reverse,
    io,
    sync::mpsc::{self, Receiver, TryRecvError},
    thread::{self, sleep},
    time::{Duration, Instant, SystemTime},
};

use board::{Board, HASH_VALUES};
use clap::Parser;
use log::{debug, error, info, warn};
use move_generator::{
    can_capture_opponent_king, generate_moves, generate_moves_without_history, perft, PerftStats,
    ENABLE_UNMAKE_MOVE_TEST,
};
use moves::{square_indices_to_moves, Move, MoveRollback};
use search::DEFAULT_HISTORY_TABLE;
use uci::UciInterface;

use num_format::{Locale, ToFormattedString};

mod board;
mod evaluate;
mod move_generator;
mod moves;
mod repetition_tracker;
mod search;
mod transposition_table;
mod uci;

pub static STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(Parser)]
struct CliArgs {
    #[arg(short, long, default_value_t = log::LevelFilter::Debug)]
    log_level: log::LevelFilter,
}

fn main() {
    let args = CliArgs::parse();

    let setup_logger_result = setup_logger(&args);
    if setup_logger_result.is_err() {
        panic!("logger setup failed: {}", setup_logger_result.unwrap_err())
    }
    log_panics::init();

    // dereference lazy cell to cause it to initialize
    let _ = *HASH_VALUES;

    run_uci();

    // print_moves_from_pos("4k3/8/8/8/3qbrp1/3QPB2/4N3/4K3 w - - 0 1");
    // do_perft(5, STARTING_FEN);
    // do_perft(4, "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // do_perft(6, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 1 1");
    // do_perft(5, "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1");
    // do_perft(4, "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
    // do_perft(4, "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10");
    // make_moves(Vec::from([ Move { data: 0x0040 }, Move { data: 0x4397 }, Move { data: 0x0144 }, Move { data: 0xc14e } ]), "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // run_to_pos(Vec::from([(12, 28), (52, 36), (3, 39), (57, 42), (5, 26), (62, 45), (39, 53)]));
    // hash_values_edit_distance();
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
            stats.nodes.to_formatted_string(&Locale::en),
            (nps as u64).to_formatted_string(&Locale::en)
        );
        info!("{:?}", stats);
        debug_assert!(rollback.is_empty());
    }
}

fn print_moves_from_pos(fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    info!("{:?}", &board);

    let mut moves = generate_moves_without_history(&mut board);

    moves.sort_by_key(|m| Reverse(m.score));

    for r#move in moves {
        info!(
            "{} move order score {}",
            r#move.m.pretty_print(Some(&board)),
            r#move.score
        );
    }
}

fn run_to_pos(index_moves: Vec<(u8, u8, Option<u16>)>) {
    let moves = square_indices_to_moves(index_moves);
    let mut board = Board::from_fen(STARTING_FEN).unwrap();
    let mut rollback = MoveRollback::default();
    for r#move in moves {
        board.make_move(&r#move.m, &mut rollback);
    }

    let final_pos_moves = generate_moves_without_history(&mut board);

    if final_pos_moves.is_empty() {
        warn!("No moves found")
    }

    for r#move in final_pos_moves {
        info!("{}", r#move.m.pretty_print(Some(&board)));
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

    let final_pos_moves = generate_moves_without_history(&mut board);

    if final_pos_moves.is_empty() {
        warn!("No moves found")
    }

    for r#move in final_pos_moves {
        info!("{}", r#move.m.pretty_print(Some(&board)));
    }
}

fn setup_logger(args: &CliArgs) -> Result<(), fern::InitError> {
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
        .level(args.log_level)
        // .level_for("fox_chess::move_generator", log::LevelFilter::Trace)
        .chain(std::io::stderr())
        .chain(fern::log_file("output.log")?)
        .apply()?;
    Ok(())
}

fn run_uci() {
    if ENABLE_UNMAKE_MOVE_TEST {
        error!("Running UCI with ENABLE_UNMAKE_MOVE_TEST enabled. Performance will be degraded heavily.")
    }

    // 2^23 entries -> 128MiB
    let mut uci = UciInterface::new(23);
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

fn hash_values_edit_distance() {
    let hash_values = &*HASH_VALUES;
    let mut total_distance = 0;
    let mut total_count = 0;
    let mut pawn_distance = 0;
    let mut pawn_count = 0;

    for x in 8..hash_values.len() {
        // skip unused pawn values
        if (x >= 56 && x <= 63) || (x >= 6 * 64 && x <= 6 * 64 + 7) || (x >= 6 * 64 + 56 && x <= 6 * 64 + 63) {
            continue;
        }

        for y in (x + 1)..hash_values.len() {
            if (y >= 56 && y <= 63) || (y >= 6 * 64 && y <= 6 * 64 + 7) || (y >= 6 * 64 + 56 && y <= 6 * 64 + 63) {
                continue;
            }

            let edit_distance = (hash_values[x] ^ hash_values[y]).count_ones();
            total_distance += edit_distance;
            total_count += 1;

            if (x <= 63 || (x >= 6 * 64 && x <= 6 * 64 + 63)) && (y <= 63 || (y >= 6 * 64 && y <= 6 * 64 + 63)) {
                pawn_distance += edit_distance;
                pawn_count += 1;
            }
        }
    }

    let total_avg_distance = total_distance as f64 / total_count as f64;
    let pawn_avg_distance = pawn_distance as f64 / pawn_count as f64;

    debug!("total_avg_distance: {total_avg_distance:.2}, pawn_avg_distance: {pawn_avg_distance:.2}")
}
