use std::time::Instant;

use log::info;
use num_format::{Locale, ToFormattedString};

use crate::{
    board::Board,
    moves::{MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_PROMOTION, MOVE_KING_CASTLE, MOVE_QUEEN_CASTLE, Move},
    repetition_tracker::RepetitionTracker,
    staged_move_generator::StagedMoveGenerator,
};

impl Board {
    pub fn start_perft(&mut self, depth: u8, divide: bool) -> u64 {
        let mut stats = PerftStats::default();
        let mut repetitions = RepetitionTracker::default();

        let start_time = Instant::now();
        do_perft(depth, 1, self, &mut stats, &mut repetitions, divide);
        let elapsed = start_time.elapsed();

        if divide {
            println!("\n{}", stats.nodes);
        }

        let nps = stats.nodes as f64 / elapsed.as_secs_f64();
        info!(
            "depth {depth} in {elapsed:#?}. Nodes: {}. Nodes per second: {}",
            stats.nodes.to_formatted_string(&Locale::en),
            (nps as u64).to_formatted_string(&Locale::en)
        );
        info!("{:?}", stats);

        stats.nodes
    }
}

#[derive(Debug, Default)]
pub struct PerftStats {
    pub nodes: u64,
    pub captures: u64,
    pub en_passants: u64,
    pub castles: u64,
    pub promotions: u64,
    #[cfg(feature = "perft_track_checks")]
    pub checks: u64,
    #[cfg(feature = "perft_track_checks")]
    pub checkmates: u64,
}

// Code referenced from https://www.chessprogramming.org/Perft
fn do_perft(
    draft: u8,
    ply: u8,
    board: &mut Board,
    stats: &mut PerftStats,
    repetitions: &mut RepetitionTracker,
    divide: bool,
) {
    if draft == 0 {
        stats.nodes += 1;
        return;
    }

    let mut move_generator = StagedMoveGenerator::new();
    move_generator.generate_moves_pseudo_legal(board);
    while let Some(mov) = move_generator.get_next_move_unordered(board) {
        let mut new_board = board.clone();
        let (legal, move_made) = new_board.test_legality_and_maybe_make_move(mov, repetitions);
        if !legal {
            if move_made {
                repetitions.unmake_move(new_board.hash);
            }
            continue;
        }

        if draft == 1 {
            check_perft_stats(mov, &new_board, stats, repetitions);
        }

        let start_nodes = stats.nodes;
        do_perft(draft - 1, ply + 1, &mut new_board, stats, repetitions, divide);

        if divide && ply == 1 {
            println!("{} {}", mov.simple_long_algebraic_notation(), stats.nodes - start_nodes)
        }

        repetitions.unmake_move(new_board.hash);
    }
}

// Allow unused variables because the board and repetitions parameters are used when the feature perft_track_checks is enabled
#[allow(unused_variables)]
fn check_perft_stats(mov: Move, board: &Board, stats: &mut PerftStats, repetitions: &mut RepetitionTracker) {
    let flags = mov.data >> 12;
    if flags & MOVE_FLAG_CAPTURE != 0 {
        stats.captures += 1;

        if flags == MOVE_EP_CAPTURE {
            stats.en_passants += 1;
        }
    } else if flags == MOVE_KING_CASTLE || flags == MOVE_QUEEN_CASTLE {
        stats.castles += 1;
    }

    if flags & MOVE_FLAG_PROMOTION != 0 {
        stats.promotions += 1;
    }

    #[cfg(feature = "perft_track_checks")]
    if board.is_in_check(false) {
        stats.checks += 1;

        let mut checkmate = true;

        let mut move_generator = StagedMoveGenerator::new();
        move_generator.generate_moves_check_evasion(board, None, None, None, None, None);
        while let Some(mov) = move_generator.get_next_move_unordered(board) {
            let mut new_board = board.clone();
            let (legal, move_made) = new_board.test_legality_and_maybe_make_move(mov, repetitions);

            if move_made {
                repetitions.unmake_move(new_board.hash);
            }

            if legal {
                checkmate = false;
                break;
            }
        }

        if checkmate {
            stats.checkmates += 1;
        }
    }
}
