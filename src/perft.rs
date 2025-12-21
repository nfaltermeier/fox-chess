use std::time::Instant;

use arrayvec::ArrayVec;
use log::info;
use num_format::{Locale, ToFormattedString};

use crate::{
    board::Board,
    move_generator_struct::{GetMoveResult, MoveGenerator},
    moves::{
        MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_FLAG_PROMOTION, MOVE_KING_CASTLE, MOVE_QUEEN_CASTLE, Move,
        MoveRollback,
    },
};

const ENABLE_PERFT_STATS: bool = true;
/// This option is slow
const ENABLE_PERFT_STATS_CHECKS: bool = false;
/// This option is very slow
const ENABLE_PERFT_STATS_CHECKMATES: bool = false;

impl Board {
    pub fn start_perft(&mut self, depth: u8, divide: bool) -> u64 {
        let mut rollback = MoveRollback::default();
        let mut stats = PerftStats::default();

        let start_time = Instant::now();
        do_perft(depth, 1, self, &mut rollback, &mut stats, divide);
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
        assert!(rollback.is_empty());

        stats.nodes
    }
}

#[derive(Debug, Default)]
pub struct PerftStats {
    pub nodes: u64,
    pub captures: u64,
    pub eps: u64,
    pub castles: u64,
    pub promotions: u64,
    pub checks: u64,
    pub checkmates: u64,
}

// Code referenced from https://www.chessprogramming.org/Perft
fn do_perft(draft: u8, ply: u8, board: &mut Board, rollback: &mut MoveRollback, stats: &mut PerftStats, divide: bool) {
    if draft == 0 {
        let mut moves = ArrayVec::new();
        // slow as all heck
        if ENABLE_PERFT_STATS && ENABLE_PERFT_STATS_CHECKMATES {
            board.generate_legal_moves_without_history(&mut moves);
            if moves.is_empty() {
                stats.checkmates += 1;
            }
        }

        stats.nodes += 1;
        return;
    }

    let mut move_generator = MoveGenerator::new();
    move_generator.generate_moves_pseudo_legal(board);
    loop {
        let r#move = match move_generator.get_next_move_unordered() {
            GetMoveResult::Move(scored_move) => scored_move,
            GetMoveResult::GenerateMoves => {
                move_generator.generate_more_moves(board, None, None, None, None);
                continue;
            }
            GetMoveResult::NoMoves => break,
        };

        let (legal, move_made) = board.test_legality_and_maybe_make_move(r#move.m, rollback);
        if !legal {
            if move_made {
                board.unmake_move(&r#move.m, rollback);
            }

            continue;
        }

        if ENABLE_PERFT_STATS && draft == 1 {
            check_perft_stats(&r#move.m, board, stats);
        }

        let start_nodes = stats.nodes;
        do_perft(draft - 1, ply + 1, board, rollback, stats, divide);

        if divide && ply == 1 {
            println!(
                "{} {}",
                r#move.m.simple_long_algebraic_notation(),
                stats.nodes - start_nodes
            )
        }

        board.unmake_move(&r#move.m, rollback);
    }
}

fn check_perft_stats(r#move: &Move, board: &mut Board, stats: &mut PerftStats) {
    let flags = r#move.data >> 12;
    if flags & MOVE_FLAG_CAPTURE != 0 {
        stats.captures += 1;

        if flags == MOVE_EP_CAPTURE {
            stats.eps += 1;
        }
    } else if flags == MOVE_KING_CASTLE || flags == MOVE_QUEEN_CASTLE {
        stats.castles += 1;
    }

    if flags & MOVE_FLAG_PROMOTION != 0 {
        stats.promotions += 1;
    }

    board.white_to_move = !board.white_to_move;
    if ENABLE_PERFT_STATS_CHECKS && board.can_capture_opponent_king(false) {
        stats.checks += 1;
    }
    board.white_to_move = !board.white_to_move;
}
