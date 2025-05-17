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
        179,228,196,80,76,55,13,83,
        10,47,-7,30,15,59,94,-6,
        -11,-6,-13,11,17,17,-1,-3,
        -14,7,-6,5,-1,3,0,-14,
        -22,8,1,-5,-2,5,23,-2,
        -29,-8,-21,-11,-12,1,26,-16,
        0,0,0,0,0,0,0,0,
        -157,-13,-13,-13,33,-42,-44,-101,
        -27,-2,21,55,27,39,-16,10,
        -16,12,33,54,84,84,41,14,
        14,3,26,41,8,47,7,34,
        0,-7,13,0,13,23,24,-2,
        -1,-1,4,17,25,2,14,-1,
        -28,-12,-2,2,5,4,-22,1,
        -73,-6,-37,-32,0,-7,-11,-41,
        -31,-35,-18,-31,-13,-50,4,-6,
        -24,-24,10,11,2,0,3,-22,
        -10,-13,18,38,41,66,18,27,
        18,-7,8,26,24,15,-8,7,
        -9,9,1,10,16,6,4,3,
        5,5,4,5,0,2,7,25,
        11,-8,17,-6,3,-1,15,-12,
        -47,-17,-10,4,-13,-8,-3,-23,
        36,63,53,45,73,75,87,14,
        28,32,70,65,59,95,63,59,
        21,67,54,37,47,64,74,49,
        11,6,6,21,39,37,40,13,
        -21,2,0,15,11,6,22,-2,
        -13,7,0,-3,-3,2,26,4,
        -24,-13,-1,-3,-9,14,5,-36,
        -14,-1,9,9,8,0,-21,-42,
        -1,8,35,41,58,81,98,39,
        -17,-33,7,2,21,68,29,67,
        8,9,-1,13,44,84,77,62,
        -7,-7,-15,9,2,1,35,28,
        -2,-4,-6,0,-6,8,20,14,
        -4,3,-1,5,9,4,22,4,
        -13,3,1,5,10,20,-9,9,
        14,7,-9,2,3,-11,-24,-54,
        -28,-38,-40,-48,-45,-38,-39,-30,
        -30,-31,-31,-49,-51,-43,-2,-31,
        -29,29,-11,-26,10,70,122,14,
        -25,53,8,12,33,99,130,39,
        -14,31,35,30,16,-2,-34,5,
        -13,32,18,10,0,-12,-1,-30,
        9,18,7,-23,-10,-21,6,16,
        -9,35,14,-33,25,-29,26,20,
        0,0,0,0,0,0,0,0,
        319,301,326,145,153,240,239,311,
        94,47,70,37,63,26,65,69,
        52,49,50,-2,-13,1,37,13,
        26,26,14,-22,6,12,21,7,
        29,24,12,21,11,13,3,3,
        35,30,23,73,8,18,6,5,
        0,0,0,0,0,0,0,0,
        -50,-35,-26,-42,-30,-27,-40,-53,
        -48,-34,-58,-40,-29,-64,-21,-67,
        -14,-8,-43,-45,-102,-42,-29,-62,
        -63,-30,-48,-41,-19,-47,-36,-61,
        -61,-43,-13,-7,-18,-37,-22,-58,
        -84,-55,-40,-47,-37,-38,-64,-67,
        -41,-23,-47,-66,-57,-73,-45,-145,
        -100,-83,-42,-69,-90,-57,-46,-48,
        -19,-23,-16,-21,-11,-11,-12,-28,
        -9,-19,-42,-37,-13,-32,-31,-43,
        -31,-10,-30,-64,-60,-50,-15,-55,
        -54,-17,-34,-33,-27,-24,-21,-47,
        -34,-23,-17,-16,-30,-36,-29,-66,
        -66,-35,-11,-35,-17,-31,-87,-90,
        -78,-62,-87,-44,-63,-51,-64,-181,
        -37,-28,-90,-64,-43,-68,-59,-37,
        31,17,17,28,-4,2,19,34,
        15,36,12,20,15,-11,19,1,
        32,4,26,21,21,0,11,-1,
        21,38,42,37,19,21,28,14,
        32,20,51,26,32,33,3,15,
        12,0,17,23,27,16,-22,-15,
        20,16,24,11,17,-16,-15,5,
        12,8,17,29,1,14,33,11,
        26,74,47,72,-3,10,-6,-13,
        46,109,108,102,142,88,76,4,
        15,7,76,62,99,47,0,-11,
        66,58,74,73,119,103,57,22,
        30,50,78,70,105,99,24,-5,
        -5,34,77,45,32,70,6,-10,
        -1,4,45,6,11,-25,-45,-5,
        -14,-28,-4,9,-6,1,-5,-5,
        -49,-22,-10,-15,-12,26,-6,-56,
        -29,46,2,43,61,68,47,-1,
        4,23,27,29,38,19,7,42,
        -21,-10,20,11,5,-15,-28,-4,
        -26,-5,1,5,14,18,15,10,
        3,-17,-3,5,9,18,7,6,
        -45,-8,0,17,10,13,-6,-23,
        -28,-62,-27,-27,-70,-10,-54,-68,
        0,76,316,340,498,1012,20000,8,
        16,
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
            if c2.contains("M") || c2.contains("/1;") || c2.contains("/1 ") {
                continue;
            }
        }

        let c0_index = line.find("c0");
        if c0_index.is_none() {
            panic!("Could not find c0, possibly a malformed line: {}", line);
        }
        let c0_index = c0_index.unwrap();

        let fen = &line[..c0_index];

        if STARTING_FEN.starts_with(fen) {
            continue;
        }

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
