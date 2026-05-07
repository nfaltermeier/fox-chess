#[cfg(feature = "pgn")]
use std::path::Path;
use std::sync::mpsc;

#[cfg(feature = "pgn")]
use clap::Args;
use clap::{Parser, Subcommand};

use log::error;
use vampirc_uci::parse_with_unknown;

#[cfg(feature = "pgn")]
use crate::pgn::{print_epds_for_pgn, print_tuning_positions, reprint_pgns};
use crate::{STARTING_FEN, bench, board::Board, uci::UciInterface};

#[derive(Parser)]
pub struct CliArgs {
    #[command(subcommand)]
    pub command: Option<Command>,
    /// Controls what level of debugging information is logged. Defaults to Error.
    #[arg(long)]
    pub log_level: Option<log::LevelFilter>,
    /// Controls if debugging information will be logged to the file output.log
    #[arg(long)]
    pub log_to_file: bool,
}

#[derive(Subcommand)]
pub enum Command {
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
pub enum PgnCommands {
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
    pub filepath: String,
    /// Skips using games that end in a KBNvK draw
    #[arg(long)]
    pub include_kbnk_draws: bool,
    /// Allows printing moves where the comment contains 'book'
    #[arg(long)]
    pub include_book_moves: bool,
    /// Allows printing moves with no comments
    #[arg(long)]
    pub include_no_comment_moves: bool,
    /// Additionally prints the move's score from the original game
    #[arg(long)]
    pub print_score: bool,
    #[command(flatten)]
    pub sampling: RandomSampling,
    #[arg(long)]
    pub win_adj_moves: Option<u16>,
    #[arg(long)]
    pub win_adj_score: Option<i16>,
    #[arg(long)]
    pub draw_adj_moves: Option<u16>,
    #[arg(long)]
    pub draw_adj_score: Option<i16>,
    #[arg(long)]
    pub draw_adj_min_ply: Option<u16>,
}

#[cfg(feature = "pgn")]
#[derive(Args)]
#[group(required = true, multiple = false)]
pub struct RandomSampling {
    /// set the number of positions to randomly sample (approximately)
    #[arg(long = "positions")]
    pub positions_to_choose: u32,

    /// Disables random sampling
    #[arg(long = "no-sampling")]
    pub no_random_sampling: bool,
}

pub fn handle_startup_command(command: &Command) {
    match command {
        &Command::Perft {
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
                    let mut uci = UciInterface::new(10, stop_rx);
                    uci.process_command((uci_command.clone(), messages));

                    uci.get_board_copy().unwrap().start_perft(depth, true);
                    return;
                }

                error!("If the depth perft argument is provided, fen must also be provided");
            }
        }
        Command::Bench => {
            bench();
        }
        Command::Version => {
            println!("{}", UciInterface::get_version());
        }
        #[cfg(feature = "pgn")]
        Command::Pgn(subcommand) => match subcommand {
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

fn perft_test_position(fen: &str, expected_results: Vec<(u8, u64)>) {
    let mut board = Board::from_fen(fen, None).unwrap();

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
