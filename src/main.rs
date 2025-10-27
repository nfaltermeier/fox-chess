#![feature(core_intrinsics)]

use std::{
    cmp::Reverse,
    sync::mpsc::{self, TryRecvError},
    thread::sleep,
    time::{Duration, SystemTime},
};

use crate::bench::bench;
use board::{Board, HASH_VALUES};
use clap::{Parser, Subcommand};
use log::{debug, error, info, warn};
use magic_bitboard::initialize_magic_bitboards;
use move_generator::ENABLE_UNMAKE_MOVE_TEST;
use moves::{Move, MoveRollback};
use search::{DEFAULT_HISTORY_TABLE, SearchResult, Searcher};
use texel::{find_best_params, load_positions};
use transposition_table::TranspositionTable;
use uci::UciInterface;
use vampirc_uci::{UciSearchControl, parse_with_unknown};

mod bench;
mod bitboard;
mod board;
mod evaluate;
mod magic_bitboard;
mod move_generator;
mod moves;
mod repetition_tracker;
mod search;
mod texel;
mod transposition_table;
mod uci;

pub static STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(Parser)]
struct CliArgs {
    #[command(subcommand)]
    command: Option<Commands>,
    #[arg(short, long, default_value_t = log::LevelFilter::Debug)]
    log_level: log::LevelFilter,
}

#[derive(Subcommand)]
enum Commands {
    /// Runs all perft tests if no arguments are provided. Arguments are intended for use with perftree https://github.com/agausmann/perftree
    Perft {
        depth: Option<u8>,
        fen: Option<String>,
        moves: Option<String>,
    },
    Bench,
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
    let positions = load_positions("set_three_positions.epd");
    println!(
        "{} Positions loaded, take to skip ratio: {}/{}",
        positions.positions.len(),
        positions.loaded_ratio,
        positions.skipped_ratio
    );
    // find_scaling_constant(positions.positions);
    find_best_params(positions.positions);

    // search_moves_from_pos(STARTING_FEN, 1);
    // print_moves_from_pos("rnbqkbnr/pp1ppppp/8/2p5/1P6/8/P1PPPPPP/RNBQKBNR w KQkq - 0 2");
    // make_moves(Vec::from([ Move { data: 0x0040 }, Move { data: 0x4397 }, Move { data: 0x0144 }, Move { data: 0xc14e } ]), "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1");
    // hash_values_edit_distance();
}

fn handle_startup_command(command: &Commands) {
    match command {
        &Commands::Perft {
            depth,
            ref fen,
            ref moves,
        } => {
            let has_any_subarg = depth.is_some() || fen.is_some() || moves.is_some();

            if !has_any_subarg {
                run_perft_tests();
            } else {
                if let Some(depth) = depth {
                    if let Some(fen) = fen
                        && fen != ""
                    {
                        let mut uci_command = format!("position fen {fen}");
                        if let Some(moves) = moves
                            && moves != ""
                        {
                            uci_command = format!("{uci_command} moves {moves}");
                        }

                        let messages = parse_with_unknown(&uci_command);
                        let (_, stop_rx) = mpsc::channel::<()>();
                        let mut uci = UciInterface::new(2, stop_rx);
                        uci.process_command((uci_command.clone(), messages));

                        uci.get_board_copy().unwrap().start_perft(depth, true);
                        return;
                    }
                }

                error!("If the depth perft argument is provided, fen must also be provided")
            }
        }
        Commands::Bench => {
            bench();
        }
    }
}

fn do_perfts_up_to(up_to_depth: u8, fen: &str) {
    let mut board = Board::from_fen(fen).unwrap();
    for depth in 1..=up_to_depth {
        board.start_perft(depth, false);
    }
}

fn perft_test_position(fen: &str, expected_results: Vec<(u8, u64)>) {
    let mut board = Board::from_fen(fen).unwrap();

    for (depth, nodes) in expected_results {
        assert_eq!(nodes, board.start_perft(depth, false))
    }
}

fn run_perft_tests() {
    perft_test_position(
        STARTING_FEN,
        vec![
            (1, 20),
            (2, 400),
            (3, 8902),
            (4, 197_281),
            (5, 4_865_609),
            (6, 119_060_324),
        ],
    );
    perft_test_position(
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 1",
        vec![(1, 48), (2, 2039), (3, 97_862), (4, 4_085_603), (5, 193_690_690)],
    );
    perft_test_position(
        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 1 1",
        vec![(1, 14), (2, 191), (3, 2812), (4, 43_238), (5, 674_624), (6, 11_030_083)],
    );
    perft_test_position(
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
        vec![(1, 6), (2, 264), (3, 9467), (4, 422333), (5, 15_833_292)],
    );
    perft_test_position(
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
        vec![(1, 44), (2, 1486), (3, 62_379), (4, 2_103_487), (5, 89_941_194)],
    );
    perft_test_position(
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        vec![(1, 46), (2, 2079), (3, 89_890), (4, 3_894_594), (5, 164_075_551)],
    );
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
    let mut continuation_history = [[[[[0; 64]; 6]; 64]; 6]; 2];

    for r#move in moves {
        info!("{}:", r#move.m.pretty_print(Some(&board)));
        board.make_move(&r#move.m, &mut rollback);

        let (_, stop_rx) = mpsc::channel::<()>();
        let mut searcher = Searcher::new(
            &mut board,
            &mut transposition_table,
            &mut history,
            &stop_rx,
            &mut continuation_history,
        );

        let mut result;
        if depth != 1 {
            let tc = None;
            let sc = Some(UciSearchControl::depth(depth - 1));

            result = searcher.iterative_deepening_search(&tc, &sc);
            result.best_move = r#move.m;
        } else {
            result = SearchResult {
                best_move: r#move.m,
                score: 0,
            };
        }

        board.unmake_move(&r#move.m, &mut rollback);

        if best.as_ref().is_none_or(|v| {
            v.score * if board.white_to_move { 1 } else { -1 } < result.score * if board.white_to_move { 1 } else { -1 }
        }) {
            best = Some(result);
        }
    }

    if let Some(r) = best {
        debug!(
            "best move: {} eval: {}",
            r.best_move.pretty_print(Some(&board)),
            r.score
        )
    } else {
        debug!("No valid moves");
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
    let (message_rx, stop_rx) = UciInterface::process_stdin_uci();
    let mut uci = UciInterface::new(23, stop_rx);
    loop {
        match message_rx.try_recv() {
            Ok(val) => {
                if uci.process_command(val) {
                    return;
                }
            }
            Err(TryRecvError::Empty) => {}
            Err(TryRecvError::Disconnected) => {
                error!("stdin channel disconnected");
                panic!("stdin channel disconnected")
            }
        }
        sleep(Duration::from_millis(10));
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
