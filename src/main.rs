use std::{
    sync::mpsc::TryRecvError,
    thread::sleep,
    time::{Duration, SystemTime},
};

use bench::bench;
use board::ZOBRIST_HASH_VALUES;
use build_info::build_info;
use clap::Parser;
use log::error;
use magic_bitboard::initialize_magic_bitboards;
use uci::UciInterface;

use cli::{CliArgs, handle_startup_command};

mod bench;
mod bitboard;
mod board;
mod cli;
mod correction_history;
mod eval_values;
mod evaluate;
mod magic_bitboard;
mod move_generator;
mod moves;
mod perft;
#[cfg(feature = "pgn")]
mod pgn;
mod pretty_print_stats;
mod repetition_tracker;
mod search;
mod staged_move_generator;
mod time_management;
mod transposition_table;
mod uci;
mod uci_required_options_helper;

pub static STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

build_info!(fn get_build_info);

fn main() {
    let args = CliArgs::parse();

    let setup_logger_result = setup_logger(&args);
    if let Err(setup_logger_result) = setup_logger_result {
        panic!("logger setup failed: {}", setup_logger_result)
    }
    log_panics::init();

    // dereference lazy cell to cause it to initialize
    let _ = *ZOBRIST_HASH_VALUES;
    initialize_magic_bitboards();

    if let Some(command) = &args.command {
        handle_startup_command(command);
        return;
    }

    run_uci();
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
