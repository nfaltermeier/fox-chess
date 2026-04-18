#[cfg(feature = "pgn")]
use std::path::Path;
use std::{
    sync::mpsc::{self, TryRecvError},
    thread::sleep,
    time::{Duration, SystemTime},
};

use bench::bench;
use board::{Board, HASH_VALUES};
use build_info::build_info;
#[cfg(feature = "pgn")]
use clap::Args;
use clap::{Parser, Subcommand};
use log::{debug, error};
use magic_bitboard::initialize_magic_bitboards;
use move_generator::ENABLE_UNMAKE_MOVE_TEST;
#[cfg(feature = "pgn")]
use pgn::{print_epds_for_pgn, print_tuning_positions, reprint_pgns};
use uci::UciInterface;
use vampirc_uci::parse_with_unknown;

mod bench;
mod bitboard;
mod board;
mod eval_values;
mod evaluate;
mod magic_bitboard;
mod move_generator;
mod move_generator_struct;
mod moves;
mod perft;
#[cfg(feature = "pgn")]
mod pgn;
mod repetition_tracker;
mod search;
mod time_management;
mod transposition_table;
mod uci;
mod uci_required_options_helper;

pub static STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

build_info!(fn get_build_info);

#[derive(Parser)]
struct CliArgs {
    #[command(subcommand)]
    command: Option<Commands>,
    /// Controls what level of debugging information is logged. Defaults to Error.
    #[arg(long)]
    log_level: Option<log::LevelFilter>,
    /// Controls if debugging information will be logged to the file output.log
    #[arg(long)]
    log_to_file: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Runs all perft tests if no arguments are provided. Arguments are intended for use with perftree https://github.com/agausmann/perftree
    Perft {
        depth: Option<u8>,
        fen: Option<String>,
        moves: Option<String>,
    },
    /// Runs a benchmark test for the user to verify node count and nodes per second on a new device/build
    Bench,
    /// Prints the version of the program
    Version,
    /// Tools for processing PGN files
    #[cfg(feature = "pgn")]
    #[command(subcommand)]
    Pgn(PgnCommands),
}

#[cfg(feature = "pgn")]
#[derive(Subcommand)]
enum PgnCommands {
    /// Prints positions from the PGN file's games in an EPD format. By default only includes games that terminated normally.
    Epd {
        /// Path of the input PGN file
        filepath: String,
        /// Includes games regardless of the Termination tag value. Normally only the 'normal' value is allowed.
        #[arg(long)]
        include_all_terminations: bool,
    },
    /// Prints selected quiet positions from the PGN file's games for tuning evaluation (gradient-tuning branch)
    Tuning(TuningArgs),
    Reprint {
        /// Path of the input PGN file
        filepath: String,
    },
}

#[cfg(feature = "pgn")]
#[derive(Args)]
pub struct TuningArgs {
    /// Path of the input PGN file
    filepath: String,
    /// Skips using games that end in a KBNvK draw
    #[arg(long)]
    include_kbnk_draws: bool,
    /// Allows printing moves where the comment contains 'book'
    #[arg(long)]
    include_book_moves: bool,
    /// Allows printing moves with no comments
    #[arg(long)]
    include_no_comment_moves: bool,
    /// Additionally prints the move's score from the original game
    #[arg(long)]
    print_score: bool,
    #[command(flatten)]
    sampling: RandomSampling,
    #[arg(long)]
    win_adj_moves: Option<u16>,
    #[arg(long)]
    win_adj_score: Option<i16>,
    #[arg(long)]
    draw_adj_moves: Option<u16>,
    #[arg(long)]
    draw_adj_score: Option<i16>,
    #[arg(long)]
    draw_adj_min_ply: Option<u16>,
}

#[cfg(feature = "pgn")]
#[derive(Args)]
#[group(required = true, multiple = false)]
pub struct RandomSampling {
    /// set the number of positions to randomly sample (approximately)
    #[arg(long = "positions")]
    positions_to_choose: u32,

    /// Disables random sampling
    #[arg(long = "no-sampling")]
    no_random_sampling: bool,
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

    if let Some(command) = &args.command {
        handle_startup_command(command);
        return;
    }

    run_uci();

    // hash_values_edit_distance();
}

fn setup_logger(args: &CliArgs) -> Result<(), fern::InitError> {
    let mut logger = fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{} {} {}] {}",
                humantime::format_rfc3339_seconds(SystemTime::now()),
                record.level(),
                record.target(),
                message
            ))
        })
        .level(args.log_level.unwrap_or_else(|| {
            if get_build_info().profile.eq_ignore_ascii_case("debug") {
                log::LevelFilter::Debug
            } else {
                log::LevelFilter::Error
            }
        }))
        .level_for("fox_chess::perft", log::LevelFilter::Info)
        .chain(std::io::stderr());

    if args.log_to_file || get_build_info().profile.eq_ignore_ascii_case("debug") {
        logger = logger.chain(fern::log_file("output.log")?);
    }

    logger.apply()?;
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
                if let Some(depth) = depth
                    && let Some(fen) = fen
                    && !fen.is_empty()
                {
                    let mut uci_command = format!("position fen {fen}");
                    if let Some(moves) = moves
                        && !moves.is_empty()
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

                error!("If the depth perft argument is provided, fen must also be provided");
            }
        }
        Commands::Bench => {
            bench();
        }
        Commands::Version => {
            println!("{}", UciInterface::get_version());
        }
        #[cfg(feature = "pgn")]
        Commands::Pgn(subcommand) => match subcommand {
            PgnCommands::Epd {
                filepath,
                include_all_terminations,
            } => {
                print_epds_for_pgn(Path::new(filepath), *include_all_terminations);
            }
            PgnCommands::Tuning(args) => {
                if args.win_adj_score.is_some_and(|s| s < 0) {
                    eprintln!("win_adj_score must be greater or equal to 0 when provided");
                    return;
                } else if args.draw_adj_score.is_some_and(|s| s < 0) {
                    eprintln!("draw_adj_score must be greater or equal to 0 when provided");
                    return;
                } else if (args.draw_adj_min_ply.is_some()
                    || args.draw_adj_moves.is_some()
                    || args.draw_adj_score.is_some())
                    && !(args.draw_adj_min_ply.is_some()
                        && args.draw_adj_moves.is_some()
                        && args.draw_adj_score.is_some())
                {
                    eprintln!("Draw adjudication will have no effect if not all arguments are provided");
                    return;
                } else if (args.win_adj_moves.is_some() || args.win_adj_score.is_some())
                    && !(args.win_adj_moves.is_some() && args.win_adj_score.is_some())
                {
                    eprintln!("Score adjudication will have no effect if not all arguments are provided");
                    return;
                }

                print_tuning_positions(args);
            }
            PgnCommands::Reprint { filepath } => {
                reprint_pgns(Path::new(filepath));
            }
        },
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
