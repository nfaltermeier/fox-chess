#![feature(core_intrinsics)]

use std::{
    cmp::Reverse,
    io,
    sync::mpsc::{self, Receiver, TryRecvError},
    thread::{self, sleep},
    time::{Duration, SystemTime},
};

use board::{Board, HASH_VALUES};
use clap::Parser;
use log::{debug, error, info, warn};
use magic_bitboard::initialize_magic_bitboards;
use move_generator::ENABLE_UNMAKE_MOVE_TEST;
use moves::{square_indices_to_moves, Move, MoveRollback};
use search::{SearchResult, Searcher, DEFAULT_HISTORY_TABLE};
use texel::{find_best_params, find_scaling_constant, load_positions, DEFAULT_PARAMS};
use transposition_table::TranspositionTable;
use uci::UciInterface;
use vampirc_uci::UciSearchControl;

mod bitboard;
mod board;
mod evaluate;
mod magic_bitboard;
mod move_generator;
mod moves;
mod repetition_tracker;
mod search;
mod transposition_table;
mod uci;
mod texel;

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
    initialize_magic_bitboards();

    if ENABLE_UNMAKE_MOVE_TEST {
        error!("Running with ENABLE_UNMAKE_MOVE_TEST enabled. Performance will be degraded heavily.")
    }

    // rayon::ThreadPoolBuilder::new().num_threads(8).build_global().unwrap();

    println!("Loading positions from file");
    let positions = load_positions("new-positions.epd");
    println!("{} Positions loaded", positions.len());
    // find_scaling_constant(positions);
    find_best_params(positions);

    // run_uci();

    // search_moves_from_pos(STARTING_FEN, 1);
    // print_moves_from_pos("rnbqkbnr/pp1ppppp/8/2p5/1P6/8/P1PPPPPP/RNBQKBNR w KQkq - 0 2");
    // do_perfts_up_to(5, STARTING_FEN);
    // do_perfts_up_to(4, "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // do_perfts_up_to(6, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 1 1");
    // do_perfts_up_to(5, "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1");
    // do_perfts_up_to(4, "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
    // do_perfts_up_to(4, "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10");
    // make_moves(Vec::from([ Move { data: 0x0040 }, Move { data: 0x4397 }, Move { data: 0x0144 }, Move { data: 0xc14e } ]), "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // run_to_pos(Vec::from([(12, 28), (52, 36), (3, 39), (57, 42), (5, 26), (62, 45), (39, 53)]));
    // hash_values_edit_distance();
}

fn do_perfts_up_to(up_to_depth: u8, fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    for depth in 1..=up_to_depth {
        board.start_perft(depth, false);
    }
}

fn print_moves_from_pos(fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    info!("{:#?}", &board);

    let mut moves = board.generate_legal_moves_without_history();

    moves.sort_by_key(|m| Reverse(m.score));

    for r#move in moves {
        info!(
            "{} move order score {}",
            r#move.m.pretty_print(Some(&board)),
            r#move.score
        );
    }
}

fn search_moves_from_pos(fen: &str, depth: u8) {
    let mut board = Board::from_fen(fen).unwrap();
    info!("{:#?}", &board);

    let mut moves = board.generate_legal_moves_without_history();

    moves.sort_by_key(|m| Reverse(m.score));

    let mut rollback = MoveRollback::default();
    let mut transposition_table = TranspositionTable::new(23);
    let mut history = DEFAULT_HISTORY_TABLE;
    let mut best: Option<SearchResult> = None;

    for r#move in moves {
        info!("{}:", r#move.m.pretty_print(Some(&board)));
        board.make_move(&r#move.m, &mut rollback);

        let mut searcher = Searcher::new(&mut board, &mut transposition_table, &mut history);

        let mut result;
        if depth != 1 {
            let tc = None;
            let sc = Some(UciSearchControl::depth(depth - 1));

            result = searcher.iterative_deepening_search(&tc, &sc);
            result.best_move = r#move.m;
        } else {
            result = SearchResult {
                best_move: r#move.m,
                eval: 0,
            };
        }

        board.unmake_move(&r#move.m, &mut rollback);

        if best.as_ref().is_none_or(|v| v.eval * if board.white_to_move { 1 } else { -1 } < result.eval * if board.white_to_move { 1 } else { -1 }) {
            best = Some(result);
        }
    }

    if let Some(r) = best {
        debug!("best move: {} eval: {}", r.best_move.pretty_print(Some(&board)), r.eval)
    } else {
        debug!("No valid moves");
    }
}

fn run_to_pos(index_moves: Vec<(u8, u8, Option<u16>)>) {
    let moves = square_indices_to_moves(index_moves);
    let mut board = Board::from_fen(STARTING_FEN).unwrap();
    let mut rollback = MoveRollback::default();
    for r#move in moves {
        board.make_move(&r#move.m, &mut rollback);
    }

    let final_pos_moves = board.generate_legal_moves_without_history();

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
        let legal = !board.can_capture_opponent_king(true);
        debug!("legality: {}", legal)
    }

    debug!("After all moves");
    debug!("{:?}", board);

    let final_pos_moves = board.generate_legal_moves_without_history();

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
