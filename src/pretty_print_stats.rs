use std::time::Duration;

use tinyvec::TinyVec;

use crate::{
    board::{Board, PIECE_MASK, PIECE_PAWN, file_8x8, piece_to_letter, rank_8x8},
    evaluate::{MATE_THRESHOLD, MATE_VALUE},
    moves::{MOVE_FLAG_CAPTURE_FULL, MOVE_FLAG_PROMOTION, MOVE_KING_CASTLE, MOVE_QUEEN_CASTLE, Move},
    repetition_tracker::RepetitionTracker,
    search::SearchStats,
    staged_move_generator::StagedMoveGenerator,
    transposition_table::TranspositionTable,
};

pub fn print_header() {
    println!("  d/sd pv#  score       time nodes nodes/s hashfull pv");
}

#[inline(never)]
pub fn pretty_print_stats(
    score: i16,
    stats: &SearchStats,
    elapsed: &Duration,
    transposition_table: &TranspositionTable,
    pv_moves: &TinyVec<[Move; 32]>,
    search_starting_fullmove: u8,
    multi_pv: u8,
    selective_depth: u8,
    board: &Board,
) {
    let abs_score = score.abs();
    let score_string = if abs_score >= MATE_THRESHOLD {
        let diff = MATE_VALUE - abs_score;
        let moves = (diff as f32 / 20.0).ceil();
        let mate_str = format!("{}M{moves}", if score < 0 { "-" } else { "" });
        format!("{mate_str:>6}")
    } else {
        let pawns = score as f32 / 100.0;
        format!("{pawns:>6.2}")
    };

    let time = {
        let total_millis = elapsed.as_millis();
        let hours = total_millis / 3600000;
        let minutes = (total_millis % 3600000) / 60000;
        let seconds = (total_millis % 60000) / 1000;
        let millis = total_millis % 1000;
        let without_hours = format!("{minutes:02}:{seconds:02}.{millis:03}");
        if hours > 0 {
            format!("{hours}:{without_hours}")
        } else {
            without_hours
        }
    };

    let depth = stats.depth;

    let total_nodes = stats.current_iteration_total_nodes + stats.previous_iterations_total_nodes;
    let nodes_str = format_nodes(total_nodes);

    let nps = (total_nodes as f32 / elapsed.as_secs_f32()).round() as u64;
    let nps_str = format!("{}/s", format_nodes(nps));

    let hashfull = transposition_table.hashfull(search_starting_fullmove) as f32 / 10.0;

    let pv_str = format_moves_san(board, pv_moves);

    println!("{depth:>3}/{selective_depth:<3} {multi_pv:>2} {score_string}  {time} {nodes_str} {nps_str} {hashfull:>7.1}% {pv_str}");
}

fn format_moves_san(board: &Board, moves: &TinyVec<[Move; 32]>) -> String {
    let mut board = board.clone();
    let mut repetitions = RepetitionTracker::default();
    let mut result = String::new();

    for (move_index, mov) in moves.iter().rev().enumerate() {
        let to = mov.to();
        let from = mov.from() as u8;
        let moving_piece_type = board.get_piece_64(mov.from() as usize) & PIECE_MASK;

        let mut alternate_moves = Vec::new();

        if mov.flags() == MOVE_KING_CASTLE {
            result.push_str("0-0");
        } else if mov.flags() == MOVE_QUEEN_CASTLE {
            result.push_str("0-0-0");
        } else {
            let mut move_generator = StagedMoveGenerator::new();
            move_generator.generate_moves_pseudo_legal(&board);
            while let Some(generated_move) = move_generator.get_next_move_unordered(&board) {
                // Exclude when the PV move and the generated move has the same from so alternate promotions don't trigger disambiguation
                if generated_move != *mov
                    && generated_move.from() as u8 != from
                    && generated_move.to() == to
                    && board.get_piece_64(generated_move.from() as usize) & PIECE_MASK == moving_piece_type
                {
                    alternate_moves.push(generated_move);
                }
            }

            if moving_piece_type != PIECE_PAWN {
                result.push(piece_to_letter(moving_piece_type));
            }

            if !alternate_moves.is_empty() {
                let from_file = file_8x8(from);
                let from_rank = rank_8x8(from);

                let mut file_match = false;
                let mut rank_match = false;

                for alt_mov in alternate_moves {
                    let alt_from_file = file_8x8(alt_mov.from() as u8);
                    let alt_from_rank = rank_8x8(alt_mov.from() as u8);

                    file_match |= alt_from_file == from_file;
                    rank_match |= alt_from_rank == from_rank;
                }

                if !file_match {
                    result.push((b'a' + from_file) as char);
                } else if !rank_match {
                    result.push((b'0' + from_rank) as char);
                } else {
                    result.push((b'a' + from_file) as char);
                    result.push((b'0' + from_rank) as char);
                }
            } else if moving_piece_type == PIECE_PAWN && mov.data & MOVE_FLAG_CAPTURE_FULL != 0 {
                let from_file = file_8x8(from);
                result.push((b'a' + from_file) as char);
            }

            if mov.data & MOVE_FLAG_CAPTURE_FULL != 0 {
                result.push('x');
            }

            let to_rank = rank_8x8(to as u8);
            let to_file = file_8x8(to as u8);

            result.push((b'a' + to_file) as char);
            result.push((b'0' + to_rank) as char);

            let flags = mov.flags();
            if flags & MOVE_FLAG_PROMOTION != 0 {
                let promo_value = (flags as u8) & 3;
                let promo_to_piece = promo_value + 2;

                result.push('=');
                result.push(piece_to_letter(promo_to_piece));
            }
        }

        let (legal, _) = board.test_legality_and_maybe_make_move(*mov, &mut repetitions);
        if !legal {
            panic!("Failed to make move {} when replaying PV", mov.pretty_print(None))
        }

        if board.is_in_check(false) {
            // Assuming search works then checkmate is only possible as the last move in the PV
            let mut checkmate = false;

            if move_index + 1 == moves.len() {
                // assume checkmate until disproven
                checkmate = true;

                let mut move_generator = StagedMoveGenerator::new();
                move_generator.generate_moves_check_evasion(&board, None, None, None, None, None);
                while let Some(generated_move) = move_generator.get_next_move_unordered(&board) {
                    let mut new_board = board.clone();
                    let (legal, _) = new_board.test_legality_and_maybe_make_move(generated_move, &mut repetitions);
                    if legal {
                        checkmate = false;
                        break;
                    }
                }
            }

            if checkmate {
                result.push('#');
            } else {
                result.push('+');
            }
        }

        if move_index + 1 != moves.len() {
            result.push(' ');
        }
    }

    result
}

fn format_nodes(nodes: u64) -> String {
    let levels = [
        ('T', 1_000_000_000_000),
        ('B', 1_000_000_000),
        ('M', 1_000_000),
        ('K', 1_000),
    ];

    for (signifier, value) in levels {
        if nodes >= value {
            let multiples = nodes as f32 / value as f32;
            let log = multiples.log10().floor() as usize;
            let decimals_places = 2 - log;
            return format!("{multiples:>4.decimals_places$}{signifier}");
        }
    }

    format!("{nodes:>5}")
}
