use std::{
    cmp::Reverse,
    sync::mpsc::{self, TryRecvError},
    thread::sleep,
    time::{Duration, Instant, SystemTime},
};

use board::{Board, HASH_VALUES};
use clap::Parser;
use log::{debug, error, info, warn};
use move_generator::{
    can_capture_opponent_king, generate_legal_moves_without_history, perft, PerftStats,
    ENABLE_UNMAKE_MOVE_TEST,
};
use moves::{square_indices_to_moves, Move, MoveRollback};
use search::DEFAULT_HISTORY_TABLE;
use transposition_table::TranspositionTable;
use uci::UciInterface;

use num_format::{Locale, ToFormattedString};
use vampirc_uci::UciSearchControl;

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

    // search_moves_from_pos("4k1K1/R7/P3P3/8/8/r7/8/8 w - - 45 104", 9);
    // do_perft(5, STARTING_FEN);
    // do_perft(5, "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1");
    // do_perft(4, "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // do_perft(6, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 1 1");
    // do_perft(5, "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1");
    // do_perft(4, "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
    // do_perft(4, "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10");
    // make_moves(Vec::from([ Move { data: 0x0040 }, Move { data: 0x4397 }, Move { data: 0x0144 }, Move { data: 0xc14e } ]), "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // run_to_pos(Vec::from([(12, 28), (52, 36), (3, 39), (57, 42), (5, 26), (62, 45), (39, 53)]));
    // hash_values_edit_distance();
    // print_moves_from_pos("3k1b1r/1R5p/3p1p2/3N1P2/2n2p2/3Q3P/2P3P1/6K1 b - - 0 30");
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
        // .level_for("fox_chess::search", log::LevelFilter::Trace)
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
    let (message_rx, stop_rx) = UciInterface::process_stdin_uci();
    let mut uci = UciInterface::new(23, stop_rx);
    loop {
        match message_rx.try_recv() {
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

fn search_moves_from_pos(fen: &str, depth: u8) {
    let mut board = Board::from_fen(fen).unwrap();
    info!("{:#?}", &board);

    let mut moves = generate_legal_moves_without_history(&mut board);

    moves.sort_by_key(|m| Reverse(m.score));

    let mut rollback = MoveRollback::default();
    let mut transposition_table = TranspositionTable::new(23);
    let mut history = DEFAULT_HISTORY_TABLE;
    let (_, rx) = mpsc::channel::<()>();
    for r#move in moves {
        info!("{}:", r#move.m.pretty_print(Some(&board)));
        board.make_move(&r#move.m, &mut rollback);

        let tc = None;
        let sc = Some(UciSearchControl::depth(depth));

        board.iterative_deepening_search(&tc, &sc, &mut transposition_table, &mut history, &rx);

        board.unmake_move(&r#move.m, &mut rollback);
    }
}

fn print_moves_from_pos(fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    info!("{:#?}", &board);

    let mut moves = generate_legal_moves_without_history(&mut board);

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

    let final_pos_moves = generate_legal_moves_without_history(&mut board);

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
    debug!("{:?}", board);

    for r#move in moves {
        debug!("{}", r#move.pretty_print(Some(&board)));
        board.make_move(&r#move, &mut rollback);
        let legal = !can_capture_opponent_king(&board, true);
        debug!("legality: {}", legal)
    }

    debug!("After all moves");
    debug!("{:?}", board);

    let final_pos_moves = generate_legal_moves_without_history(&mut board);

    if final_pos_moves.is_empty() {
        warn!("No moves found")
    }

    for r#move in final_pos_moves {
        info!("{}", r#move.m.pretty_print(Some(&board)));
    }
}

fn hash_values_edit_distance() {
    let hash_values = &*HASH_VALUES;
    let mut total_distance = 0;
    let mut total_count = 0;
    let mut pawn_distance = 0;
    let mut pawn_count = 0;

    for x in 8..hash_values.len() {
        // skip unused pawn values
        if (56..=63).contains(&x) || (6 * 64..=6 * 64 + 7).contains(&x) || (6 * 64 + 56..=6 * 64 + 63).contains(&x) {
            continue;
        }

        for y in (x + 1)..hash_values.len() {
            if (56..=63).contains(&y) || (6 * 64..=6 * 64 + 7).contains(&y) || (6 * 64 + 56..=6 * 64 + 63).contains(&y)
            {
                continue;
            }

            let edit_distance = (hash_values[x] ^ hash_values[y]).count_ones();
            total_distance += edit_distance;
            total_count += 1;

            if (x <= 63 || (6 * 64..=6 * 64 + 63).contains(&x)) && (y <= 63 || (6 * 64..=6 * 64 + 63).contains(&y)) {
                pawn_distance += edit_distance;
                pawn_count += 1;
            }
        }
    }

    let total_avg_distance = total_distance as f64 / total_count as f64;
    let pawn_avg_distance = pawn_distance as f64 / pawn_count as f64;

    debug!("total_avg_distance: {total_avg_distance:.2}, pawn_avg_distance: {pawn_avg_distance:.2}")
}
