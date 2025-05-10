use std::{fs::File, io::{BufRead, BufReader}, time::{Instant, SystemTime, UNIX_EPOCH}};
use std::io::Write;

#[allow(internal_features)]
use std::intrinsics::fadd_algebraic;

use rayon::prelude::*;

use crate::{board::{Board, PIECE_KING}, moves::MoveRollback, search::{HistoryTable, Searcher, DEFAULT_HISTORY_TABLE}, transposition_table::TranspositionTable};

pub struct TexelPosition {
    pub board: Board,
    pub result: f32,
}

pub const EP_PIECE_VALUES_IDX: usize = 768;
pub const EP_DOUBLED_PAWNS_IDX: usize = 775;
pub static DEFAULT_PARAMS: [i16; 776] = [
        // pawn midgame
        0,  0,  0,  0,  0,  0,  0,  0,
        50, 50, 50, 50, 50, 50, 50, 50,
        10, 10, 20, 30, 30, 20, 10, 10,
        5,  5, 10, 25, 25, 10,  5,  5,
        0,  0,  0, 20, 20,  0,  0,  0,
        5, -5,-10,  0,  0,-10, -5,  5,
        5, 10, 10,-20,-20, 10, 10,  5,
        0,  0,  0,  0,  0,  0,  0,  0,
        // knight midgame
        -50,-40,-30,-30,-30,-30,-40,-50,
        -40,-20,  0,  0,  0,  0,-20,-40,
        -30,  0, 10, 15, 15, 10,  0,-30,
        -30,  5, 15, 20, 20, 15,  5,-30,
        -30,  0, 15, 20, 20, 15,  0,-30,
        -30,  5, 10, 15, 15, 10,  5,-30,
        -40,-20,  0,  5,  5,  0,-20,-40,
        -50,-40,-30,-30,-30,-30,-40,-50,
        // bishop midgame
        -20,-10,-10,-10,-10,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5, 10, 10,  5,  0,-10,
        -10,  5,  5, 10, 10,  5,  5,-10,
        -10,  0, 10, 10, 10, 10,  0,-10,
        -10, 10, 10, 10, 10, 10, 10,-10,
        -10,  5,  0,  0,  0,  0,  5,-10,
        -20,-10,-10,-10,-10,-10,-10,-20,
        // rook midgame
        0,  0,  0,  0,  0,  0,  0,  0,
        5, 10, 10, 10, 10, 10, 10,  5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        0,  0,  0,  5,  5,  0,  0,  0,
        // queen midgame
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5,  5,  5,  5,  0,-10,
        -5,  0,  5,  5,  5,  5,  0, -5,
        0,  0,  5,  5,  5,  5,  0, -5,
        -10,  5,  5,  5,  5,  5,  0,-10,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -20,-10,-10, -5, -5,-10,-10,-20,
        // king midgame
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -20,-30,-30,-40,-40,-30,-30,-20,
        -10,-20,-20,-20,-20,-20,-20,-10,
        20, 20,  0,  0,  0,  0, 20, 20,
        20, 30, 10,  0,  0, 10, 30, 20,
        // pawn endgame
        0,  0,  0,  0,  0,  0,  0,  0,
        50, 50, 50, 50, 50, 50, 50, 50,
        30, 30, 30, 30, 30, 30, 30, 30,
        20, 20, 20, 20, 20, 20, 20, 20,
        10, 10, 10, 10, 10, 10, 10, 10,
        0,  0,  0,  0,  0,  0,  0,  0,
        0,  0,  0,  0,  0,  0,  0,  0,
        0,  0,  0,  0,  0,  0,  0,  0,
        // knight endgame
        -50,-40,-30,-30,-30,-30,-40,-50,
        -40,-20,  0,  0,  0,  0,-20,-40,
        -30,  0, 10, 15, 15, 10,  0,-30,
        -30,  5, 15, 20, 20, 15,  5,-30,
        -30,  0, 15, 20, 20, 15,  0,-30,
        -30,  5, 10, 15, 15, 10,  5,-30,
        -40,-20,  0,  5,  5,  0,-20,-40,
        -50,-40,-30,-30,-30,-30,-40,-50,
        // bishop endgame
        -20,-10,-10,-10,-10,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5, 10, 10,  5,  0,-10,
        -10,  5,  5, 10, 10,  5,  5,-10,
        -10,  0, 10, 10, 10, 10,  0,-10,
        -10, 10, 10, 10, 10, 10, 10,-10,
        -10,  5,  0,  0,  0,  0,  5,-10,
        -20,-10,-10,-10,-10,-10,-10,-20,
        // rook endgame
        0,  0,  0,  0,  0,  0,  0,  0,
        5, 10, 10, 10, 10, 10, 10,  5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        0,  0,  0,  5,  5,  0,  0,  0,
        // queen endgame
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5,  5,  5,  5,  0,-10,
        -5,  0,  5,  5,  5,  5,  0, -5,
        0,  0,  5,  5,  5,  5,  0, -5,
        -10,  5,  5,  5,  5,  5,  0,-10,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -20,-10,-10, -5, -5,-10,-10,-20,
        // king endgame
        -50,-40,-30,-20,-20,-30,-40,-50,
        -30,-20,-10,  0,  0,-10,-20,-30,
        -30,-10, 20, 30, 30, 20,-10,-30,
        -30,-10, 30, 40, 40, 30,-10,-30,
        -30,-10, 30, 40, 40, 30,-10,-30,
        -30,-10, 20, 30, 30, 20,-10,-30,
        -30,-30,  0,  0,  0,  0,-30,-30,
        -50,-30,-30,-30,-30,-30,-30,-50,
        // piece values
        0, 100, 315, 350, 500, 900, 20000,
        // doubled pawns
        25
    ];

pub fn load_positions(filename: &str) -> Vec<TexelPosition> {
    let mut result = vec![];

    let file = File::open(filename).unwrap();

    for line in BufReader::new(file).lines() {
        let line = line.unwrap();

        if line.len() == 0 {
            continue;
        }

        let c2_index = line.find("c2");
        // The first positions are when it left book but they lack a comment
        if let Some(c2_index) = c2_index {
            let c2 = &line[c2_index..];

            // If forced mate was found or evaluation was skipped because only one move was possible (which may indicate the player is being mated)
            if c2.contains("M") || c2.contains("/1") {
                continue;
            }
        }

        let c0_index = line.find("c0");
        if c0_index.is_none() {
            panic!("Could not find c0, possibly a malformed line: {}", line);
        }
        let c0_index = c0_index.unwrap();

        let fen = &line[..c0_index];
        let board = Board::from_fen(fen).unwrap();

        let c1_index = line.find("c1");
        if c1_index.is_none() {
            panic!("Could not find c1, possibly a malformed line: {}", line);
        }
        let c1_index = c1_index.unwrap();

        let c1_and_remaining = &line[c1_index + 3..];

        let match_result = &c1_and_remaining[..(c1_and_remaining.find(";").unwrap())];
        let match_result_value = match match_result {
            "1-0" => 1.0,
            "1/2-1/2" => 0.5,
            "0-1" => 0.0,
            _ => panic!("Unexpected match result {match_result} on line {line}")
        };

        result.push(TexelPosition { board, result: match_result_value });
    }

    result
}

fn sigmoid(eval: f32, scaling_constant: f32) -> f32 {
    let exp = -eval * scaling_constant / 400.0;
    1.0 / (1.0 + 10.0_f32.powf(exp))
}

pub fn find_scaling_constant(mut positions: Vec<TexelPosition>) {
    let evals = positions.par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let eval = p.board.quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, &DEFAULT_PARAMS, r).0 as f32;
            (p.result, eval * if p.board.white_to_move { 1.0 } else { -1.0 })
        })
        .collect::<Vec<(f32, f32)>>();

    let mut scaling_constant = 1.06;
    let mut best_error = find_error_from_evals(&evals, scaling_constant);

    let mut improving = true;
    while improving {
        improving = false;

        scaling_constant += 0.01;
        let new_error = find_error_from_evals(&evals, scaling_constant);
        if new_error < best_error {
            best_error = new_error;
            improving = true;
        } else {
            scaling_constant -= 0.02;
            let new_error = find_error_from_evals(&evals, scaling_constant);
            if new_error < best_error {
                best_error = new_error;
                improving = true;
            } else {
                scaling_constant += 0.01;
            }
        }
    }

    println!("Best scaling constant is {scaling_constant}")
}

fn find_error_from_evals(evals: &Vec<(f32, f32)>, scaling_constant: f32) -> f32 {
    let errors = evals.par_iter()
        .map(|e| {
            let val_sqrt = e.0 - sigmoid(e.1, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f32>>();

    sum_orlp(&errors[..]) / evals.len() as f32
}

pub fn find_best_params(mut nonquiet_positions: Vec<TexelPosition>) {
    let mut params = DEFAULT_PARAMS.clone();

    let scaling_constant = 1.06;
    let mut best_error = f32::NAN;

    let mut improving = true;
    let mut count = 0;
    let mut adjustments: u64 = 0;
    while improving {
        improving = false;

        let quiet_positions;
        (best_error, quiet_positions) = find_quiet_positions_and_error(&mut nonquiet_positions, &params, scaling_constant);
        println!("Starting new loop, new best error is {best_error}");

        for i in 0..params.len() {
            // midgame pawns on first row
            if i < 8
                // midgame pawns on last row
                || (i >= 56 && i < 64)
                // endgame pawns on first row
                || (i >= 6 * 64 && i < 8 + 6 * 64)
                // endgame pawns on last row
                || (i >= 56 + 6 * 64 && i < 64 + 6 * 64)
                // None piece centipawn value
                || i == EP_PIECE_VALUES_IDX
                // King centipawn value
                || i == EP_PIECE_VALUES_IDX + PIECE_KING as usize
            {
                continue;
            }

            params[i] += 1;
            let new_error = find_error_for_quiet_positions(&quiet_positions, &params, scaling_constant);
            if new_error < best_error {
                best_error = new_error;
                improving = true;
                adjustments += 1;

                if adjustments % 1000 == 999 {
                    println!("Saving, error: {best_error}, adjustments: {adjustments}, time: {}", humantime::format_rfc3339(SystemTime::now()));
                    save_params(&params);
                }

                let mut local_improving = true;
                while local_improving {
                    params[i] += 1;
                    let new_error = find_error_for_quiet_positions(&quiet_positions, &params, scaling_constant);
                    local_improving = new_error < best_error;
                    if local_improving {
                        best_error = new_error;
                        adjustments += 1;

                        if adjustments % 1000 == 999 {
                            println!("Saving, error: {best_error}, adjustments: {adjustments}, time: {}", humantime::format_rfc3339(SystemTime::now()));
                            save_params(&params);
                        }
                    }
                }

                params[i] -= 1;
            } else {
                params[i] -= 2;
                let new_error = find_error_for_quiet_positions(&quiet_positions, &params, scaling_constant);
                if new_error < best_error {
                    best_error = new_error;
                    improving = true;
                    adjustments += 1;

                    if adjustments % 1000 == 999 {
                        println!("Saving, error: {best_error}, adjustments: {adjustments}, time: {}", humantime::format_rfc3339(SystemTime::now()));
                        save_params(&params);
                    }

                    let mut local_improving = true;
                    while local_improving {
                        params[i] -= 1;
                        let new_error = find_error_for_quiet_positions(&quiet_positions, &params, scaling_constant);
                        local_improving = new_error < best_error;
                        if local_improving {
                            best_error = new_error;
                            adjustments += 1;

                            if adjustments % 1000 == 999 {
                                println!("Saving, error: {best_error}, adjustments: {adjustments}, time: {}", humantime::format_rfc3339(SystemTime::now()));
                                save_params(&params);
                            }
                        }
                    }

                    params[i] += 1;
                } else {
                    params[i] += 1;
                }
            }
        }

        count += 1;
        println!("Saving, error: {best_error}, iterations: {count}, adjustments: {adjustments}, time: {}", humantime::format_rfc3339(SystemTime::now()));
        save_params(&params);
    }

    println!("Regression done");
}

fn search_error_for_params(positions: &mut Vec<TexelPosition>, params: &[i16; 776], scaling_constant: f32) -> f32 {
    let errors = positions.par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p.board.quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r);

            let eval = (result.0 * if p.board.white_to_move { 1 } else { -1 }) as f32;
            let val_sqrt = p.result - sigmoid(eval, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f32>>();

    sum_orlp(&errors[..]) / positions.len() as f32
}

fn find_quiet_positions_and_error(positions: &mut Vec<TexelPosition>, params: &[i16; 776], scaling_constant: f32) -> (f32, Vec<TexelPosition>) {
    let mut quiet_positions = vec![];
    let mut errors = vec![];

    positions.par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p.board.quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r);
            let qp = TexelPosition {
                board: result.1,
                result: p.result,
            };

            let eval = (result.0 * if p.board.white_to_move { 1 } else { -1 }) as f32;
            let val_sqrt = p.result - sigmoid(eval, scaling_constant);
            (qp, val_sqrt * val_sqrt)
        })
        .unzip_into_vecs(&mut quiet_positions, &mut errors);

    (sum_orlp(&errors[..]) / positions.len() as f32, quiet_positions)
}

fn find_error_for_quiet_positions(quiet_positions: &Vec<TexelPosition>, params: &[i16; 776], scaling_constant: f32) -> f32 {
    let errors = quiet_positions.par_iter()
        .map(|p| {
            let val_sqrt = p.result - sigmoid(p.board.evaluate(params) as f32, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f32>>();

    sum_orlp(&errors[..]) / errors.len() as f32
}

fn save_params(params: &[i16; 776]) {
    let mut f = File::create(format!("params/{}.txt", SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis())).unwrap();
    write!(f, "[").unwrap();

    for (i, v) in params.iter().enumerate() {
        if i % 10 == 9 {
            writeln!(f, "{v},").unwrap();
        } else {
            write!(f, "{v},").unwrap();
        }
    }
}

// Summing floats can be surprisingly complicated. These methods are taken from https://orlp.net/blog/taming-float-sums/
// which has a lovely writeup on the issue and solutions
fn sum_block(arr: &[f32]) -> f32 {
    arr.iter().fold(0.0, |x, y| fadd_algebraic(x, *y))
}

pub fn sum_orlp(arr: &[f32]) -> f32 {
    let mut chunks = arr.chunks_exact(256);
    let mut sum = 0.0;
    let mut c = 0.0;
    for chunk in &mut chunks {
        let y = sum_block(chunk) - c;
        let t = sum + y;
        c = (t - sum) - y;
        sum = t;
    }
    sum + (sum_block(chunks.remainder()) - c)
}
