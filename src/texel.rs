use std::io::Write;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    time::{SystemTime, UNIX_EPOCH},
};

#[allow(internal_features)]
use std::intrinsics::fadd_algebraic;

use rayon::prelude::*;

use crate::{
    STARTING_FEN,
    board::{Board, PIECE_KING},
    moves::MoveRollback,
};

pub struct TexelPosition {
    pub board: Board,
    pub result: f32,
}

pub type EvalParams = [i16; 779];

pub const EP_PIECE_VALUES_IDX: usize = 768;
pub const EP_DOUBLED_PAWNS_IDX: usize = 775;
pub const EP_PASSED_PAWN_IDX: usize = 776;
pub const EP_ROOK_OPEN_FILE_IDX: usize = 777;
pub const EP_ROOK_HALF_OPEN_FILE_IDX: usize = 778;

#[rustfmt::skip]
pub static DEFAULT_PARAMS: EvalParams = [
        0,0,0,0,0,0,0,0,
        138,174,122,87,78,58,13,80,
        32,58,40,46,57,59,95,10,
        -10,4,0,13,17,15,4,-3,
        -14,7,-7,5,-4,4,0,-15,
        -23,8,0,-5,-1,5,23,-2,
        -29,-4,-17,-10,-9,7,26,-18,
        0,0,0,0,0,0,0,0,
        -157,-13,-14,-13,33,-45,-40,-102,
        -29,0,21,54,35,41,-26,7,
        -8,23,40,61,85,87,42,13,
        10,7,26,49,17,50,14,36,
        -1,-3,15,7,19,24,25,-3,
        -1,-5,4,16,25,2,14,-8,
        -45,-15,-1,-2,0,3,-26,-14,
        -76,-12,-38,-33,-17,-9,-12,-41,
        -26,-32,-22,-30,-13,-48,2,-6,
        -21,-22,9,13,3,3,6,-27,
        -8,2,21,32,41,67,21,27,
        17,-4,12,29,28,17,-4,9,
        -8,9,3,18,18,6,5,-2,
        3,5,12,6,7,2,1,20,
        5,-8,8,-6,0,-2,9,-31,
        -48,-22,-18,-11,-15,-15,-7,-25,
        45,57,42,42,50,71,75,14,
        31,33,57,60,50,83,61,59,
        14,52,47,38,34,57,70,38,
        4,4,6,10,22,22,33,3,
        -19,-2,-4,4,3,1,13,-11,
        -15,-4,-7,-10,-10,-6,11,-1,
        -27,-20,-9,-11,-13,-2,-8,-42,
        -13,-5,-1,1,1,0,-19,-40,
        0,10,33,51,59,81,98,38,
        -17,-31,11,16,24,77,36,68,
        0,8,6,27,44,97,78,63,
        -4,-6,1,22,32,26,48,30,
        -2,-5,0,5,8,14,20,13,
        -11,3,4,5,9,11,21,4,
        -27,-6,2,0,7,6,-13,-8,
        11,-14,-9,0,-6,-27,-30,-54,
        -30,-39,-40,-50,-47,-39,-40,-30,
        -30,-30,-29,-50,-50,-40,-2,-30,
        -30,31,-13,-23,13,66,123,14,
        -30,57,11,15,40,99,123,31,
        -18,26,32,31,18,13,-32,3,
        -12,30,18,10,3,1,0,-27,
        10,19,7,-21,-10,-9,13,16,
        -10,40,14,-34,19,-33,26,20,
        0,0,0,0,0,0,0,0,
        221,237,255,153,153,236,237,225,
        100,132,119,88,68,32,66,69,
        64,63,52,-6,-14,1,40,12,
        26,26,14,-25,-5,12,21,7,
        29,34,12,21,10,13,3,-1,
        40,41,23,93,4,18,-5,4,
        0,0,0,0,0,0,0,0,
        -50,-35,-28,-37,-33,-30,-40,-55,
        -50,-28,-61,-42,-32,-71,-22,-70,
        -15,-5,-44,-58,-103,-56,-29,-65,
        -72,-31,-48,-48,-16,-49,-39,-77,
        -59,-41,-13,-7,-17,-39,-24,-56,
        -84,-55,-41,-49,-37,-38,-71,-69,
        -44,-20,-49,-66,-56,-72,-43,-147,
        -105,-84,-44,-71,-93,-60,-41,-51,
        -20,-25,-18,-10,-10,-10,-10,-20,
        -1,-12,-44,-39,-13,-34,-26,-43,
        -23,2,-32,-67,-63,-53,-19,-61,
        -55,-15,-34,-34,-29,-23,-21,-47,
        -33,-23,-8,-11,-30,-36,-35,-79,
        -85,-35,-9,-35,-17,-31,-88,-96,
        -92,-62,-91,-44,-63,-51,-102,-185,
        -38,-25,-90,-62,-43,-68,-56,-39,
        16,16,32,31,21,16,25,35,
        45,28,14,14,21,4,19,11,
        44,10,22,15,30,14,19,16,
        46,47,49,41,22,41,41,43,
        49,41,46,32,27,44,27,42,
        19,16,24,23,22,26,6,8,
        9,14,26,17,22,0,2,-5,
        14,18,23,34,11,12,37,18,
        15,72,53,72,-2,8,-5,-20,
        44,110,115,100,142,90,78,2,
        13,1,82,74,106,50,1,-9,
        69,60,86,72,121,107,57,22,
        31,54,79,70,110,98,23,-5,
        -8,34,89,45,32,69,5,-10,
        -10,0,45,8,11,-26,-45,-10,
        -20,-30,-10,9,-5,-11,-10,-20,
        -50,-36,-30,-20,-15,22,-20,-50,
        -30,46,2,47,53,69,45,-2,
        -4,24,35,37,37,18,3,41,
        -25,-12,21,16,9,-14,-34,-3,
        -30,-3,1,8,18,24,14,7,
        1,-16,0,5,21,18,8,6,
        -46,-9,0,20,10,13,-6,-23,
        -29,-62,-26,-27,-70,-8,-54,-68,
        0,81,309,338,501,1021,20000,14,
        9,18,17,
    ];

pub struct LoadPositionsResult {
    pub positions: Vec<TexelPosition>,
    pub loaded_ratio: i32,
    pub skipped_ratio: i32,
}

pub fn load_positions(filename: &str) -> LoadPositionsResult {
    let mut result = vec![];
    let load_positions = 2;
    let skip_positions = 4;
    let load_skip_cycle_size = load_positions + skip_positions;
    let mut considered_to_load = -1;

    let file = File::open(filename).unwrap();

    for line in BufReader::new(file).lines() {
        let line = line.unwrap();

        if line.is_empty() {
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
            panic!("Could not find c0, possibly a malformed line: {line}");
        }
        let c0_index = c0_index.unwrap();

        let fen = &line[..c0_index];

        if STARTING_FEN.starts_with(fen) {
            continue;
        }

        let board = Board::from_fen(fen).unwrap();

        considered_to_load += 1;
        if considered_to_load % load_skip_cycle_size >= load_positions {
            continue;
        }

        let c1_index = line.find("c1");
        if c1_index.is_none() {
            panic!("Could not find c1, possibly a malformed line: {line}");
        }
        let c1_index = c1_index.unwrap();

        let c1_and_remaining = &line[c1_index + 3..];

        let match_result = &c1_and_remaining[..(c1_and_remaining.find(";").unwrap())];
        let match_result_value = match match_result {
            "1-0" => 1.0,
            "1/2-1/2" => 0.5,
            "0-1" => 0.0,
            _ => panic!("Unexpected match result {match_result} on line {line}"),
        };

        result.push(TexelPosition {
            board,
            result: match_result_value,
        });
    }

    LoadPositionsResult {
        positions: result,
        loaded_ratio: load_positions,
        skipped_ratio: skip_positions,
    }
}

fn sigmoid(eval: f32, scaling_constant: f32) -> f32 {
    let exp = -eval * scaling_constant / 400.0;
    1.0 / (1.0 + 10.0_f32.powf(exp))
}

pub fn find_scaling_constant(mut positions: Vec<TexelPosition>) {
    let evals = positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let eval = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, &DEFAULT_PARAMS, r)
                .0 as f32;
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
    let errors = evals
        .par_iter()
        .map(|e| {
            let val_sqrt = e.0 - sigmoid(e.1, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f32>>();

    sum_orlp(&errors[..]) / evals.len() as f32
}

pub fn find_best_params(mut nonquiet_positions: Vec<TexelPosition>) {
    let mut params = DEFAULT_PARAMS;

    let scaling_constant = 1.06;
    let mut best_error;

    let mut improving = true;
    let mut count = 0;
    let mut adjustments: u64 = 0;
    while improving {
        improving = false;

        let quiet_positions = find_quiet_positions(&mut nonquiet_positions, &params);
        best_error = find_error_for_quiet_positions(&quiet_positions, &params, scaling_constant);
        println!("Starting new loop, new best error is {best_error:.8}");

        for mut i in 0..=98 {
            // // midgame pawns on first row
            // if i < 8
            //     // midgame pawns on last row
            //     || (i >= 56 && i < 64)
            //     // endgame pawns on first row
            //     || (i >= 6 * 64 && i < 8 + 6 * 64)
            //     // endgame pawns on last row
            //     || (i >= 56 + 6 * 64 && i < 64 + 6 * 64)
            //     // None piece centipawn value
            //     || i == EP_PIECE_VALUES_IDX
            //     // King centipawn value
            //     || i == EP_PIECE_VALUES_IDX + PIECE_KING as usize
            // {
            //     continue;
            // }

            if i == 98 {
                i = EP_PASSED_PAWN_IDX;
            } else if i == 97 {
                i = EP_DOUBLED_PAWNS_IDX;
            } else if i >= 48 {
                i += 8 + 6 * 64 - 48;
            } else {
                i += 8;
            }

            params[i] += 1;
            let new_error = find_error_for_quiet_positions(&quiet_positions, &params, scaling_constant);
            if new_error < best_error {
                best_error = new_error;
                improving = true;
                adjustments += 1;

                if adjustments % 1000 == 999 {
                    println!(
                        "Saving, error: {best_error:.8}, adjustments: {adjustments}, time: {}",
                        humantime::format_rfc3339(SystemTime::now())
                    );
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
                            println!(
                                "Saving, error: {best_error:.8}, adjustments: {adjustments}, time: {}",
                                humantime::format_rfc3339(SystemTime::now())
                            );
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
                        println!(
                            "Saving, error: {best_error:.8}, adjustments: {adjustments}, time: {}",
                            humantime::format_rfc3339(SystemTime::now())
                        );
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
                                println!(
                                    "Saving, error: {best_error:.8}, adjustments: {adjustments}, time: {}",
                                    humantime::format_rfc3339(SystemTime::now())
                                );
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
        println!(
            "Saving, error: {best_error:.8}, iterations: {count}, adjustments: {adjustments}, time: {}",
            humantime::format_rfc3339(SystemTime::now())
        );
        save_params(&params);
    }

    println!("Regression done");
}

fn search_error_for_params(positions: &mut Vec<TexelPosition>, params: &EvalParams, scaling_constant: f32) -> f32 {
    let errors = positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r);

            let eval = (result.0 * if p.board.white_to_move { 1 } else { -1 }) as f32;
            let val_sqrt = p.result - sigmoid(eval, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f32>>();

    sum_orlp(&errors[..]) / positions.len() as f32
}

fn find_quiet_positions(positions: &mut Vec<TexelPosition>, params: &EvalParams) -> Vec<TexelPosition> {
    positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r);
            TexelPosition {
                board: result.1,
                result: p.result,
            }
        })
        .collect()
}

fn find_error_for_quiet_positions(
    quiet_positions: &Vec<TexelPosition>,
    params: &EvalParams,
    scaling_constant: f32,
) -> f32 {
    let errors = quiet_positions
        .par_iter()
        .map(|p| {
            let val_sqrt = p.result - sigmoid(p.board.evaluate(params) as f32, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f32>>();

    let sum = sum_orlp(&errors[..]);
    sum / errors.len() as f32
}

fn save_params(params: &EvalParams) {
    let mut f = File::create(format!(
        "params/{}.txt",
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis()
    ))
    .unwrap();

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
