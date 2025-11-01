use std::fs;
use std::intrinsics::fmul_algebraic;
use std::io::Write;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    time::{SystemTime, UNIX_EPOCH},
};

#[allow(internal_features)]
use std::intrinsics::fadd_algebraic;

use rayon::prelude::*;
use tinyvec::ArrayVec;

use crate::{
    STARTING_FEN,
    board::{Board, PIECE_KING},
    moves::MoveRollback,
};

pub struct TexelPosition {
    pub board: Board,
    pub result: f32,
}

pub struct PositionData {
    features: ArrayVec<[(f32, u16); POSITON_MAX_FEATURES]>,
    match_result: f32,
}

pub const EVAL_PARAM_COUNT: usize = 779;
pub type EvalParams = [i16; EVAL_PARAM_COUNT];
pub type EvalFeatures = [f32; EVAL_PARAM_COUNT];
pub type EvalGradient = [f64; EVAL_PARAM_COUNT];
pub const POSITON_MAX_FEATURES: usize = 128;

pub const EP_PIECE_VALUES_IDX: usize = 768;
pub const EP_DOUBLED_PAWNS_IDX: usize = 775;
pub const EP_PASSED_PAWN_IDX: usize = 776;
pub const EP_ROOK_OPEN_FILE_IDX: usize = 777;
pub const EP_ROOK_HALF_OPEN_FILE_IDX: usize = 778;

#[rustfmt::skip]
pub static DEFAULT_PARAMS: EvalFeatures = [
        0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
        164.0,147.0,136.0,128.0,85.0,59.0,16.0,55.0,
        37.0,53.0,50.0,46.0,50.0,62.0,74.0,30.0,
        0.0,11.0,-2.0,11.0,20.0,16.0,14.0,-2.0,
        -12.0,4.0,-5.0,1.0,-1.0,10.0,6.0,-13.0,
        -13.0,2.0,-9.0,-7.0,2.0,5.0,21.0,-4.0,
        -18.0,-5.0,-18.0,-16.0,-8.0,19.0,29.0,-14.0,
        0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
        -124.0,-21.0,-7.0,-15.0,24.0,-46.0,-39.0,-118.0,
        -10.0,4.0,23.0,36.0,43.0,38.0,-35.0,-10.0,
        6.0,27.0,42.0,60.0,76.0,81.0,36.0,6.0,
        1.0,13.0,32.0,51.0,22.0,53.0,17.0,26.0,
        -7.0,7.0,21.0,8.0,20.0,18.0,26.0,-8.0,
        -23.0,-8.0,-1.0,10.0,25.0,2.0,5.0,-11.0,
        -49.0,-22.0,-15.0,-5.0,-4.0,-1.0,-13.0,-33.0,
        -81.0,-24.0,-35.0,-25.0,-24.0,-15.0,-16.0,-66.0,
        -13.0,-29.0,-36.0,-18.0,-37.0,-57.0,-14.0,20.0,
        -7.0,-1.0,7.0,-10.0,9.0,4.0,-8.0,-17.0,
        0.0,16.0,27.0,34.0,37.0,63.0,34.0,25.0,
        3.0,2.0,17.0,38.0,30.0,25.0,-4.0,-3.0,
        -10.0,11.0,6.0,30.0,19.0,5.0,0.0,1.0,
        -9.0,8.0,10.0,9.0,10.0,6.0,6.0,1.0,
        -11.0,-2.0,1.0,-5.0,2.0,-3.0,11.0,-16.0,
        -37.0,-25.0,-18.0,-19.0,-17.0,-18.0,-23.0,-25.0,
        50.0,47.0,43.0,37.0,35.0,34.0,47.0,39.0,
        24.0,27.0,45.0,55.0,46.0,67.0,54.0,53.0,
        11.0,24.0,28.0,35.0,39.0,65.0,50.0,22.0,
        -4.0,-2.0,9.0,12.0,14.0,20.0,13.0,-7.0,
        -22.0,-15.0,-8.0,-4.0,-5.0,-5.0,0.0,-22.0,
        -29.0,-20.0,-20.0,-20.0,-18.0,-19.0,-3.0,-23.0,
        -31.0,-27.0,-15.0,-17.0,-15.0,-8.0,-20.0,-49.0,
        -14.0,-14.0,-7.0,-3.0,-4.0,-5.0,-36.0,-16.0,
        4.0,21.0,28.0,39.0,67.0,78.0,71.0,61.0,
        -1.0,-8.0,21.0,30.0,35.0,73.0,48.0,66.0,
        4.0,8.0,22.0,34.0,54.0,98.0,86.0,41.0,
        -5.0,0.0,11.0,23.0,30.0,31.0,41.0,18.0,
        0.0,5.0,4.0,10.0,11.0,10.0,17.0,6.0,
        -4.0,3.0,2.0,4.0,4.0,7.0,12.0,-4.0,
        -17.0,-3.0,6.0,2.0,7.0,-4.0,-16.0,-37.0,
        -5.0,-8.0,-9.0,3.0,-9.0,-37.0,-49.0,-27.0,
        -28.0,52.0,32.0,-40.0,65.0,-14.0,51.0,-46.0,
        13.0,-4.0,58.0,-15.0,-39.0,35.0,-7.0,18.0,
        -28.0,8.0,0.0,-5.0,-28.0,-32.0,-43.0,-40.0,
        27.0,45.0,11.0,7.0,15.0,21.0,43.0,29.0,
        16.0,40.0,37.0,30.0,26.0,26.0,5.0,-2.0,
        6.0,24.0,18.0,17.0,19.0,7.0,7.0,-13.0,
        21.0,13.0,8.0,-10.0,-4.0,-2.0,19.0,18.0,
        -34.0,21.0,-2.0,-41.0,-4.0,-34.0,29.0,14.0,
        0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
        82.0,111.0,109.0,71.0,102.0,128.0,171.0,128.0,
        109.0,103.0,79.0,63.0,54.0,46.0,63.0,63.0,
        70.0,54.0,47.0,14.0,12.0,20.0,40.0,30.0,
        50.0,44.0,23.0,7.0,14.0,26.0,38.0,25.0,
        48.0,35.0,28.0,22.0,21.0,24.0,28.0,19.0,
        60.0,51.0,41.0,24.0,32.0,29.0,33.0,33.0,
        0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
        -4.0,-34.0,-34.0,-37.0,-68.0,-18.0,-31.0,-60.0,
        -64.0,-40.0,-52.0,-53.0,-56.0,-72.0,-39.0,-80.0,
        -49.0,-65.0,-36.0,-51.0,-75.0,-78.0,-53.0,-68.0,
        -72.0,-45.0,-28.0,-51.0,-25.0,-46.0,-48.0,-90.0,
        -60.0,-37.0,-25.0,-19.0,-38.0,-41.0,-75.0,-66.0,
        -85.0,-50.0,-31.0,-38.0,-43.0,-44.0,-72.0,-95.0,
        -58.0,-61.0,-55.0,-53.0,-58.0,-59.0,-71.0,-117.0,
        -105.0,-104.0,-67.0,-72.0,-91.0,-83.0,-118.0,-109.0,
        -14.0,-10.0,-18.0,-25.0,-30.0,-28.0,-47.0,-58.0,
        -37.0,-34.0,-37.0,-38.0,-39.0,-51.0,-49.0,-48.0,
        -41.0,-44.0,-47.0,-63.0,-60.0,-65.0,-58.0,-71.0,
        -50.0,-38.0,-48.0,-47.0,-45.0,-58.0,-35.0,-45.0,
        -46.0,-43.0,-35.0,-41.0,-42.0,-34.0,-38.0,-66.0,
        -81.0,-48.0,-37.0,-46.0,-29.0,-51.0,-74.0,-79.0,
        -71.0,-72.0,-62.0,-47.0,-59.0,-56.0,-85.0,-148.0,
        -68.0,-48.0,-81.0,-55.0,-71.0,-81.0,-65.0,-66.0,
        3.0,25.0,27.0,32.0,47.0,55.0,41.0,29.0,
        40.0,39.0,30.0,23.0,28.0,20.0,26.0,11.0,
        38.0,29.0,27.0,22.0,24.0,14.0,20.0,24.0,
        40.0,42.0,30.0,27.0,25.0,24.0,25.0,37.0,
        46.0,40.0,37.0,30.0,29.0,35.0,24.0,31.0,
        12.0,15.0,18.0,21.0,15.0,20.0,4.0,10.0,
        4.0,7.0,9.0,6.0,4.0,2.0,1.0,4.0,
        13.0,8.0,20.0,22.0,3.0,9.0,46.0,-5.0,
        15.0,60.0,53.0,55.0,2.0,5.0,-17.0,-20.0,
        44.0,76.0,74.0,95.0,86.0,26.0,55.0,1.0,
        14.0,37.0,55.0,75.0,73.0,1.0,-20.0,-4.0,
        51.0,33.0,81.0,72.0,75.0,82.0,54.0,23.0,
        29.0,45.0,62.0,64.0,65.0,67.0,56.0,4.0,
        -7.0,18.0,56.0,20.0,31.0,36.0,6.0,-10.0,
        -9.0,-7.0,10.0,-10.0,-7.0,-28.0,-50.0,-10.0,
        -22.0,-29.0,-11.0,-62.0,-5.0,-13.0,-10.0,-20.0,
        -69.0,-36.0,-2.0,23.0,-11.0,22.0,2.0,-64.0,
        -9.0,11.0,24.0,47.0,47.0,-9.0,36.0,21.0,
        13.0,28.0,43.0,41.0,45.0,57.0,48.0,39.0,
        -5.0,10.0,25.0,29.0,28.0,27.0,14.0,6.0,
        -28.0,-10.0,2.0,11.0,19.0,17.0,13.0,1.0,
        -37.0,-17.0,-1.0,5.0,7.0,14.0,2.0,-8.0,
        -44.0,-12.0,-9.0,2.0,2.0,5.0,-6.0,-29.0,
        6.0,-37.0,-16.0,-11.0,-28.0,-6.0,-50.0,-56.0,
        0.0,79.0,286.0,313.0,443.0,901.0,20000.0,23.0,
        8.0,21.0,18.0,
    ];

pub struct LoadPositionsResult {
    pub positions: Vec<TexelPosition>,
    pub loaded_ratio: i32,
    pub skipped_ratio: i32,
}

pub fn load_positions(filename: &str) -> LoadPositionsResult {
    let mut result = vec![];
    let load_positions = 3;
    let skip_positions = 97;
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

        considered_to_load += 1;
        if considered_to_load % load_skip_cycle_size >= load_positions {
            continue;
        }

        let board = Board::from_fen(fen).unwrap();

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

fn sigmoid(eval: f64, scaling_constant: f64) -> f64 {
    let exp = -eval * scaling_constant / 400.0;
    1.0 / (1.0 + 10.0_f64.powf(exp))
}

/// Uses gradient descent with backtracking line search
/// https://en.wikipedia.org/wiki/Gradient_descent
pub fn find_best_params(mut nonquiet_positions: Vec<TexelPosition>) {
    if !fs::exists("params").unwrap() {
        fs::create_dir("params").unwrap();
    }

    if !fs::exists("gradients").unwrap() {
        fs::create_dir("gradients").unwrap();
    }

    // test starting from zeros
    let mut params = DEFAULT_PARAMS;
    // let mut params = [0; EVAL_PARAM_COUNT];

    let scaling_constant = 1.06;
    const DEFAULT_STEP_SIZE: f64 = 10000000.0;
    let mut step_size = DEFAULT_STEP_SIZE;
    // The goal for how much to improve at each descent step
    let c = 0.01;
    // step_size is scaled by this when Armijo–Goldstein condition is not filfilled. Should be within (0, 1).
    let tau = 0.75;

    let mut iterations = 0;
    let mut changed_since_step_size_reset = false;
    let mut step_size_resets = 0;
    let mut total_param_changes = 0;
    loop {
        let quiet_position_features = find_quiet_position_features(&mut nonquiet_positions, &params);

        let base_error = find_error_for_position_data(&quiet_position_features, &params, scaling_constant);
        println!("[{}] Starting new loop, new error is {base_error:.8}", humantime::format_rfc3339(SystemTime::now()));

        let gradient = search_gradient(&quiet_position_features, &mut params, scaling_constant);
        let biggest_gradient_value = gradient.iter().map(|v| v.abs()).reduce(f64::max).unwrap();
        let avg_gradient_value = sum_orlp(&*gradient) / gradient.len() as f64;
        let mut sorted = gradient.clone();
        sorted.sort_unstable_by(f64::total_cmp);
        let median_gradient_value = sorted[gradient.len() / 2];
        println!("[{}] Calculated gradient, biggest gradient value is {biggest_gradient_value}, avg is {avg_gradient_value}, median is {median_gradient_value}", humantime::format_rfc3339(SystemTime::now()));
        save_gradient(&gradient);

        // Find appropriate learning rate https://en.wikipedia.org/wiki/Backtracking_line_search
        let m = calc_m(&gradient);
        let t = -c * m;

        let mut updated_params;
        let mut new_error = base_error;
        let mut biggest_change;
        let mut failed_on_biggest_change_one = false;
        let mut changed_params = 0;
        loop {
            updated_params = params;
            let mut changes = Vec::with_capacity(EVAL_PARAM_COUNT);
            gradient.par_iter().map(|v| (-v * step_size).round() as f32).collect_into_vec(&mut changes);
            biggest_change = changes.iter().map(|v| v.abs()).reduce(f32::max).unwrap();

            println!("[{}] Biggest change {biggest_change} for step size {step_size}", humantime::format_rfc3339(SystemTime::now()));

            // if biggest_change >= 100 {
            //     panic!("Biggest change {biggest_change} could cause an overflow")
            // } else
            if biggest_change == 0.0 {
                if changed_since_step_size_reset {
                    // Maybe the bigger derivatives have settled down now,
                    // retry from the start to give the smaller derivatives a chance to change.
                    // Params have not changed so reuse the gradient.
                    step_size = DEFAULT_STEP_SIZE;
                    changed_since_step_size_reset = false;
                    step_size_resets += 1;
                    println!("[{}] Resetting step size for the {step_size_resets}{} time", humantime::format_rfc3339(SystemTime::now()), get_ordinal_suffix(step_size_resets));

                    continue;
                } else {
                    break;
                }
            }

            updated_params.par_iter_mut().zip(&changes).for_each(|(param, change)| *param += change);

            // TODO: Searching for error shold be more accurate to the true error than reusing the found quiet positions, maybe do that? Would use a lot of memory.
            new_error = find_error_for_position_data(&quiet_position_features, &updated_params, scaling_constant);
            if base_error - new_error >= step_size * t {
                changed_params = changes.iter().filter(|v| **v != 0.0).count();
                total_param_changes += changed_params;
                changed_since_step_size_reset = true;

                break;
            } else {
                println!("[{}] Armijo-Goldstein condition failed: {} < {}", humantime::format_rfc3339(SystemTime::now()), base_error - new_error, step_size * t);

                if biggest_change == 1.0 {
                    failed_on_biggest_change_one = true;
                    break;
                }

                step_size *= tau;
            }
        }

        // To find the appropriate step size we descend the gradient, so we just use that value as our next value
        params = updated_params;

        iterations += 1;
        println!(
            "[{}] Saving, error: {new_error:.8}, iterations: {iterations}, step size: {step_size}, biggest change: {biggest_change}, changed {changed_params} params, total changed params {total_param_changes}",
            humantime::format_rfc3339(SystemTime::now())
        );
        save_features(&params);

        if failed_on_biggest_change_one || biggest_change == 0.0 {
            break;
        }
    }

    println!("Regression done");
}

fn find_quiet_position_features(positions: &mut Vec<TexelPosition>, params: &EvalFeatures) -> Vec<PositionData> {
    let mut result = Vec::with_capacity(positions.len());

    positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r);
            PositionData {
                features: result.1.get_eval_features(),
                match_result: p.result
            }
        })
        .collect_into_vec(&mut result);

    result
}

fn find_error_for_position_data(
    position_data: &Vec<PositionData>,
    params: &EvalFeatures,
    scaling_constant: f64,
) -> f64 {
    let errors = position_data
        .par_iter()
        .map(|p| {
            let val_sqrt = p.match_result as f64 - sigmoid(eval_sparse_features_algebraic_parallel(params, &p.features) as f64, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f64>>();

    let sum = sum_orlp(&errors[..]);
    sum / errors.len() as f64
}

/// params should be unchanged when this method returns
fn search_gradient(positions: &Vec<PositionData>, params: &mut EvalFeatures, scaling_constant: f64) -> Box<EvalGradient> {
    let mut result = Box::new([0f64; EVAL_PARAM_COUNT]);

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

        params[i] += 0.01;

        let positive_error = find_error_for_position_data(positions, params, scaling_constant);

        params[i] -= 0.02;

        let negative_error = find_error_for_position_data(positions, params, scaling_constant);

        params[i] += 0.01;

        // Approximate the derivative with symmetrical difference quotient numerical differentiation
        result[i] = (positive_error - negative_error) / 0.02;

        if i % 100 == 0 {
            println!("[{}] Calculated derivative for {i} elements of gradient", humantime::format_rfc3339(SystemTime::now()))
        }
    }

    result
}

/// Finds the dot product of the gradient and the search direction p
fn calc_m(gradient: &Box<EvalGradient>) -> f64 {
    // p is the negative gradient so first square everything and make them negative
    let negative_squares = gradient.par_iter().map(|v| -v * v).collect::<Vec<f64>>();
    sum_orlp(&negative_squares)
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

fn save_features(params: &EvalFeatures) {
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

fn save_gradient(gradient: &Box<EvalGradient>) {
    let mut f = File::create(format!(
        "gradients/{}.txt",
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis()
    ))
    .unwrap();

    for (i, v) in gradient.iter().enumerate() {
        if i % 8 == 7 {
            writeln!(f, "{v:018.15},").unwrap();
        } else {
            write!(f, "{v:018.15},").unwrap();
        }
    }
}

fn eval_position_features_algebraic(params: &EvalFeatures, features: &EvalFeatures) -> f32 {
    params.iter().zip(features).fold(0.0, |x, y| fadd_algebraic(fmul_algebraic(*y.0, *y.1), x))
}

fn eval_sparse_features_algebraic(params: &EvalFeatures, features: &ArrayVec<[(f32, u16); POSITON_MAX_FEATURES]>) -> f32 {
    features.iter().fold(0.0, |a, f| fadd_algebraic(fmul_algebraic(f.0, params[f.1 as usize]), a))
}

pub fn eval_sparse_features_algebraic_parallel_old(params: &EvalFeatures, features: &ArrayVec<[(f32, u16); POSITON_MAX_FEATURES]>) -> f32 {
    let mut chunks = features.chunks_exact(8);
    let summed_chunks = (&mut chunks).map(|c| c.iter().fold(0.0, |a, f| fadd_algebraic(fmul_algebraic(f.0, params[f.1 as usize]), a)));
    let chunks_sum = summed_chunks.fold(0.0, |x, y| fadd_algebraic(x, y));
    let summed_remainder = chunks.remainder().iter().fold(0.0, |a, f| fadd_algebraic(fmul_algebraic(f.0, params[f.1 as usize]), a));
    fadd_algebraic(chunks_sum, summed_remainder)
}

pub fn eval_sparse_features_algebraic_parallel(params: &EvalFeatures, features: &ArrayVec<[(f32, u16); POSITON_MAX_FEATURES]>) -> f32 {
    let mut gathered_data = [(0.0, 0.0); POSITON_MAX_FEATURES];
    for (i, f) in features.iter().enumerate() {
        gathered_data[i] = (f.0, params[f.1 as usize]);
    }

    gathered_data.iter().fold(0.0, |a, f| fadd_algebraic(fmul_algebraic(f.0, f.1), a))
}

// Summing floats can be surprisingly complicated. These methods are taken from https://orlp.net/blog/taming-float-sums/
// which has a lovely writeup on the issue and solutions
fn sum_block(arr: &[f64]) -> f64 {
    arr.iter().fold(0.0, |x, y| fadd_algebraic(x, *y))
}

pub fn sum_orlp(arr: &[f64]) -> f64 {
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

fn get_ordinal_suffix(num: i32) -> &'static str {
    match num % 10 {
        1 => "st",
        2 => "nd",
        _ => "th",
    }
}
