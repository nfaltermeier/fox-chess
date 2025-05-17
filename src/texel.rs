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

pub type EvalParams = [i16; 777];

pub const EP_PIECE_VALUES_IDX: usize = 768;
pub const EP_DOUBLED_PAWNS_IDX: usize = 775;
pub const EP_PASSED_PAWN_IDX: usize = 776;
pub static DEFAULT_PARAMS: EvalParams = [
        0,0,0,0,0,0,0,0,
        130,176,131,99,77,48,2,62,
        33,56,41,51,62,58,89,28,
        -6,5,9,16,17,15,1,-7,
        -13,6,-3,3,-2,1,0,-19,
        -19,6,4,-5,-2,7,21,-3,
        -25,-1,-11,-14,-4,11,28,-19,
        0,0,0,0,0,0,0,0,
        -152,-29,-14,-13,31,-42,-40,-87,
        -38,-7,18,54,39,41,-25,-8,
        -4,31,38,53,82,76,42,15,
        14,2,22,49,17,49,13,43,
        1,-2,17,9,23,21,23,5,
        -5,-7,4,14,24,1,15,-14,
        -55,-15,-2,-4,-4,1,-24,5,
        -76,-10,-35,-34,-22,-9,-14,-50,
        -29,-33,-30,-29,-10,-46,0,-19,
        -30,-24,9,10,6,-3,4,-34,
        -11,1,20,32,40,70,24,21,
        14,-3,12,27,26,10,-4,8,
        -12,8,1,17,18,9,6,0,
        15,6,12,4,7,2,2,18,
        15,-9,10,-5,-2,-2,9,-29,
        -53,-19,-20,-14,-15,-17,0,-18,
        36,61,53,48,75,80,84,15,
        39,38,66,73,66,93,76,56,
        26,67,60,46,46,70,73,52,
        18,15,21,25,44,41,42,22,
        -9,8,8,20,12,8,29,6,
        -7,11,3,2,0,6,33,24,
        -23,-9,0,1,-3,6,7,-36,
        -13,1,12,11,10,6,-3,-50,
        -3,13,39,47,54,77,96,31,
        -21,-35,8,16,24,75,30,65,
        -1,2,2,19,42,97,75,76,
        -2,-9,1,22,33,29,48,31,
        -7,-5,-2,9,8,13,24,18,
        -11,1,4,2,6,14,17,1,
        -14,-5,2,0,9,7,-6,-3,
        28,-15,-9,0,-9,-21,-30,-54,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-30,-39,-50,-50,-40,-2,-30,
        -30,31,-14,-24,13,66,164,17,
        -30,57,7,14,37,98,115,31,
        -20,18,17,24,16,13,-20,3,
        -13,29,15,11,-5,-4,2,-9,
        2,17,8,-21,-13,-7,16,17,
        -8,42,25,-24,39,-26,26,19,
        0,0,0,0,0,0,0,0,
        213,237,255,151,153,229,237,225,
        100,123,123,82,66,31,72,58,
        56,61,35,-11,-18,-3,37,13,
        26,18,7,-21,-14,6,21,4,
        23,25,3,17,9,14,2,-9,
        32,28,15,37,-4,5,-9,-6,
        0,0,0,0,0,0,0,0,
        -50,-38,-28,-37,-33,-30,-40,-53,
        -42,-22,-63,-31,-26,-67,-21,-65,
        -13,-5,-37,-56,-94,-53,-2,-64,
        -71,-31,-42,-53,-19,-49,-37,-76,
        -59,-41,-15,-15,-11,-38,-21,-39,
        -76,-53,-42,-46,-39,-38,-64,-63,
        -44,-20,-35,-56,-48,-68,-33,-146,
        -105,-84,-43,-71,-84,-55,-40,-50,
        -20,-22,-18,-10,-10,-10,-10,-20,
        -1,-6,-44,-39,-12,-33,-26,-37,
        -24,7,-32,-60,-61,-39,-5,-56,
        -39,-15,-32,-37,-19,-18,-16,-46,
        -33,-18,-8,-13,-30,-32,-35,-79,
        -82,-34,-6,-37,-17,-28,-87,-92,
        -101,-61,-91,-38,-62,-51,-94,-173,
        -36,-25,-85,-58,-43,-68,-56,-39,
        34,12,16,30,-3,1,17,37,
        27,36,10,19,14,-11,10,-1,
        35,-4,26,22,19,-1,10,-2,
        24,39,40,33,7,12,24,8,
        39,21,44,21,31,31,1,7,
        9,0,16,22,19,14,-40,-19,
        10,6,27,1,8,-22,-25,-5,
        8,4,15,29,1,-3,12,14,
        15,71,53,70,-2,7,-5,-20,
        44,114,115,96,142,89,74,1,
        12,1,82,74,106,50,1,-9,
        69,60,86,72,121,107,50,21,
        31,54,79,67,110,98,16,-5,
        -8,34,89,45,32,70,7,-10,
        -10,0,45,8,11,-26,-44,-10,
        -20,-27,-10,9,-5,-11,-10,-20,
        -50,-36,-30,-20,-20,0,-20,-50,
        -30,46,2,42,44,69,78,-16,
        -4,24,35,37,37,19,17,41,
        -30,-1,30,22,13,-9,-19,-1,
        -30,-3,8,9,18,25,16,7,
        -5,-16,0,6,21,20,8,4,
        -32,-8,0,20,10,8,-6,-23,
        -34,-62,-26,-25,-73,-10,-54,-60,
        0,87,309,338,502,1021,20000,13,
        8,
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
    let mut best_error;

    let mut improving = true;
    let mut count = 0;
    let mut adjustments: u64 = 0;
    while improving {
        improving = false;

        let quiet_positions = find_quiet_positions(&mut nonquiet_positions, &params);
        best_error = find_error_for_quiet_positions(&quiet_positions, &params, scaling_constant);
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

fn search_error_for_params(positions: &mut Vec<TexelPosition>, params: &EvalParams, scaling_constant: f32) -> f32 {
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

fn find_quiet_positions(positions: &mut Vec<TexelPosition>, params: &EvalParams) -> Vec<TexelPosition> {
    positions.par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p.board.quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r);
            TexelPosition {
                board: result.1,
                result: p.result,
            }
        })
        .collect()
}

fn find_error_for_quiet_positions(quiet_positions: &Vec<TexelPosition>, params: &EvalParams, scaling_constant: f32) -> f32 {
    let errors = quiet_positions.par_iter()
        .map(|p| {
            let val_sqrt = p.result - sigmoid(p.board.evaluate(params) as f32, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f32>>();

    let sum = sum_orlp(&errors[..]);
    sum / errors.len() as f32
}

fn save_params(params: &EvalParams) {
    let mut f = File::create(format!("params/{}.txt", SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis())).unwrap();

    for (i, v) in params.iter().enumerate() {
        if i % 8 == 7 {
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
