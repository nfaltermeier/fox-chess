use std::fs;
use std::io::Write;
use std::simd::i16x8;
use std::simd::num::SimdInt;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    time::{SystemTime, UNIX_EPOCH},
};

#[allow(internal_features)]
use std::intrinsics::fadd_algebraic;

use rayon::prelude::*;

use crate::evaluate::MIN_GAME_STAGE_FULLY_MIDGAME;
use crate::{
    STARTING_FEN,
    board::{Board, PIECE_KING},
    moves::MoveRollback,
};

pub struct TexelPosition {
    pub board: Board,
    pub result: f64,
}

pub const MAX_MISC_FEATURES: usize = 16;

#[derive(Default)]
pub struct FeatureData {
    pub midgame_psqt_white: [u16; 16],
    pub midgame_psqt_black: [u16; 16],
    pub endgame_psqt_white: [u16; 16],
    pub endgame_psqt_black: [u16; 16],
    pub game_stage: i16,
    pub misc_features: [(i8, u16); MAX_MISC_FEATURES],
}

pub struct PositionFeatures {
    pub features: FeatureData,
    pub result: f64,
}

pub const EVAL_PARAM_COUNT: usize = 779;
pub type EvalParams = [i16; EVAL_PARAM_COUNT];
pub type EvalGradient = [f64; EVAL_PARAM_COUNT];

pub const EP_PIECE_VALUES_IDX: usize = 768;
pub const EP_DOUBLED_PAWNS_IDX: usize = 775;
pub const EP_PASSED_PAWN_IDX: usize = 776;
pub const EP_ROOK_OPEN_FILE_IDX: usize = 777;
pub const EP_ROOK_HALF_OPEN_FILE_IDX: usize = 778;

#[rustfmt::skip]
pub static DEFAULT_PARAMS: EvalParams = [
        0,0,0,0,0,0,0,0,
        164,147,136,128,85,59,16,55,
        37,53,50,46,50,62,74,30,
        0,11,-2,11,20,16,14,-2,
        -12,4,-5,1,-1,10,6,-13,
        -13,2,-9,-7,2,5,21,-4,
        -18,-5,-18,-16,-8,19,29,-14,
        0,0,0,0,0,0,0,0,
        -124,-21,-7,-15,24,-46,-39,-118,
        -10,4,23,36,43,38,-35,-10,
        6,27,42,60,76,81,36,6,
        1,13,32,51,22,53,17,26,
        -7,7,21,8,20,18,26,-8,
        -23,-8,-1,10,25,2,5,-11,
        -49,-22,-15,-5,-4,-1,-13,-33,
        -81,-24,-35,-25,-24,-15,-16,-66,
        -13,-29,-36,-18,-37,-57,-14,20,
        -7,-1,7,-10,9,4,-8,-17,
        0,16,27,34,37,63,34,25,
        3,2,17,38,30,25,-4,-3,
        -10,11,6,30,19,5,0,1,
        -9,8,10,9,10,6,6,1,
        -11,-2,1,-5,2,-3,11,-16,
        -37,-25,-18,-19,-17,-18,-23,-25,
        50,47,43,37,35,34,47,39,
        24,27,45,55,46,67,54,53,
        11,24,28,35,39,65,50,22,
        -4,-2,9,12,14,20,13,-7,
        -22,-15,-8,-4,-5,-5,0,-22,
        -29,-20,-20,-20,-18,-19,-3,-23,
        -31,-27,-15,-17,-15,-8,-20,-49,
        -14,-14,-7,-3,-4,-5,-36,-16,
        4,21,28,39,67,78,71,61,
        -1,-8,21,30,35,73,48,66,
        4,8,22,34,54,98,86,41,
        -5,0,11,23,30,31,41,18,
        0,5,4,10,11,10,17,6,
        -4,3,2,4,4,7,12,-4,
        -17,-3,6,2,7,-4,-16,-37,
        -5,-8,-9,3,-9,-37,-49,-27,
        -28,52,32,-40,65,-14,51,-46,
        13,-4,58,-15,-39,35,-7,18,
        -28,8,0,-5,-28,-32,-43,-40,
        27,45,11,7,15,21,43,29,
        16,40,37,30,26,26,5,-2,
        6,24,18,17,19,7,7,-13,
        21,13,8,-10,-4,-2,19,18,
        -34,21,-2,-41,-4,-34,29,14,
        0,0,0,0,0,0,0,0,
        82,111,109,71,102,128,171,128,
        109,103,79,63,54,46,63,63,
        70,54,47,14,12,20,40,30,
        50,44,23,7,14,26,38,25,
        48,35,28,22,21,24,28,19,
        60,51,41,24,32,29,33,33,
        0,0,0,0,0,0,0,0,
        -4,-34,-34,-37,-68,-18,-31,-60,
        -64,-40,-52,-53,-56,-72,-39,-80,
        -49,-65,-36,-51,-75,-78,-53,-68,
        -72,-45,-28,-51,-25,-46,-48,-90,
        -60,-37,-25,-19,-38,-41,-75,-66,
        -85,-50,-31,-38,-43,-44,-72,-95,
        -58,-61,-55,-53,-58,-59,-71,-117,
        -105,-104,-67,-72,-91,-83,-118,-109,
        -14,-10,-18,-25,-30,-28,-47,-58,
        -37,-34,-37,-38,-39,-51,-49,-48,
        -41,-44,-47,-63,-60,-65,-58,-71,
        -50,-38,-48,-47,-45,-58,-35,-45,
        -46,-43,-35,-41,-42,-34,-38,-66,
        -81,-48,-37,-46,-29,-51,-74,-79,
        -71,-72,-62,-47,-59,-56,-85,-148,
        -68,-48,-81,-55,-71,-81,-65,-66,
        3,25,27,32,47,55,41,29,
        40,39,30,23,28,20,26,11,
        38,29,27,22,24,14,20,24,
        40,42,30,27,25,24,25,37,
        46,40,37,30,29,35,24,31,
        12,15,18,21,15,20,4,10,
        4,7,9,6,4,2,1,4,
        13,8,20,22,3,9,46,-5,
        15,60,53,55,2,5,-17,-20,
        44,76,74,95,86,26,55,1,
        14,37,55,75,73,1,-20,-4,
        51,33,81,72,75,82,54,23,
        29,45,62,64,65,67,56,4,
        -7,18,56,20,31,36,6,-10,
        -9,-7,10,-10,-7,-28,-50,-10,
        -22,-29,-11,-62,-5,-13,-10,-20,
        -69,-36,-2,23,-11,22,2,-64,
        -9,11,24,47,47,-9,36,21,
        13,28,43,41,45,57,48,39,
        -5,10,25,29,28,27,14,6,
        -28,-10,2,11,19,17,13,1,
        -37,-17,-1,5,7,14,2,-8,
        -44,-12,-9,2,2,5,-6,-29,
        6,-37,-16,-11,-28,-6,-50,-56,
        0,79,286,313,443,901,20000,23,
        8,21,18,
    ];

pub struct LoadPositionsResult {
    pub positions: Vec<TexelPosition>,
    pub loaded_ratio: i32,
    pub skipped_ratio: i32,
}

pub fn load_positions(filename: &str) -> LoadPositionsResult {
    let mut result = vec![];
    let load_positions = 2;
    let skip_positions = 6;
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
    let tau = 0.95;

    let mut iterations = 0;
    let mut changed_since_step_size_reset = false;
    let mut step_size_resets = 0;
    let mut total_param_changes = 0;
    loop {
        let features = qsearch_for_features(&mut nonquiet_positions, &params);

        let base_error = find_error_for_features(&features, &params, scaling_constant);
        println!("[{}] Starting new loop, new error is {base_error:.8}", humantime::format_rfc3339(SystemTime::now()));

        let gradient = eval_gradient(&features, &mut params, scaling_constant);
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
        let mut changed_params = 0;
        loop {
            updated_params = params;
            let mut changes = Vec::with_capacity(EVAL_PARAM_COUNT);
            gradient.par_iter().map(|v| (-v * step_size).round() as i16).collect_into_vec(&mut changes);
            biggest_change = changes.iter().map(|v| v.abs()).max().unwrap();

            println!("[{}] Biggest change {biggest_change} for step size {step_size}", humantime::format_rfc3339(SystemTime::now()));

            // if biggest_change >= 100 {
            //     panic!("Biggest change {biggest_change} could cause an overflow")
            // } else
            if biggest_change == 0 {
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

            // I think searching for error should be more accurate to the true error than reusing the found quiet position features but it is insanely slow, maybe because of cache stuff?
            new_error = find_error_for_features(&features, &updated_params, scaling_constant);
            if base_error - new_error >= step_size * t {
                changed_params = changes.iter().filter(|v| **v != 0).count();
                total_param_changes += changed_params;
                changed_since_step_size_reset = true;

                break;
            } else {
                println!("[{}] Armijo-Goldstein condition failed: {} < {}", humantime::format_rfc3339(SystemTime::now()), base_error - new_error, step_size * t);

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
        save_params(&params);

        if biggest_change == 0 {
            break;
        }
    }

    println!("Regression done");
}

fn search_error_for_params(positions: &mut Vec<TexelPosition>, params: &EvalParams, scaling_constant: f64) -> f64 {
    let errors = positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r).0;

            let eval = (result * if p.board.white_to_move { 1 } else { -1 }) as f64;
            let val_sqrt = p.result - sigmoid(eval, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f64>>();

    sum_orlp(&errors[..]) / positions.len() as f64
}

fn qsearch_for_features(positions: &mut Vec<TexelPosition>, params: &EvalParams) -> Vec<PositionFeatures> {
    positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r);
            PositionFeatures {
                features: result.1.get_eval_features(),
                result: p.result,
            }
        })
        .collect()
}

fn find_error_for_features(
    features: &Vec<PositionFeatures>,
    params: &EvalParams,
    scaling_constant: f64,
) -> f64 {
    let errors = features
        .par_iter()
        .map(|p| {
            let val_sqrt = p.result - sigmoid(p.features.evaluate(params) as f64, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect::<Vec<f64>>();

    let sum = sum_orlp(&errors[..]);
    sum / errors.len() as f64
}

/// params should be unchanged when this method returns
fn eval_gradient(features: &Vec<PositionFeatures>, params: &mut EvalParams, scaling_constant: f64) -> Box<EvalGradient> {
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

        params[i] += 1;

        let positive_error = find_error_for_features(features, params, scaling_constant);

        params[i] -= 2;

        let negative_error = find_error_for_features(features, params, scaling_constant);

        params[i] += 1;

        // Approximate the derivative with symmetrical difference quotient numerical differentiation
        result[i] = (positive_error - negative_error) / 2.0;

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

impl FeatureData {
    pub fn evaluate(&self, params: &EvalParams) -> i16 {
        let position_score_midgame = FeatureData::sum_psqt_for_a_phase(&self.midgame_psqt_white, &self.midgame_psqt_black, params);

        let position_score_endgame = FeatureData::sum_psqt_for_a_phase(&self.endgame_psqt_white, &self.endgame_psqt_black, params);

        let position_score_final = ((position_score_midgame * self.game_stage)
            + (position_score_endgame * (MIN_GAME_STAGE_FULLY_MIDGAME - self.game_stage)))
            / MIN_GAME_STAGE_FULLY_MIDGAME;

        let mut misc_feature_score = 0;
        for (w, i) in self.misc_features {
            misc_feature_score += w as i16 * params[i as usize];
        }

        position_score_final + misc_feature_score
    }
    
    pub fn sum_psqt_for_a_phase(white: &[u16; 16], black: &[u16; 16], params: &EvalParams) -> i16 {
        let mut mid_data = [0; 32];
        for (a_i, p_i) in white.iter().enumerate() {
            mid_data[a_i] = params[*p_i as usize];
        }
        for (a_i, p_i) in black.iter().enumerate() {
            mid_data[a_i + 16] = -params[*p_i as usize];
        }

        let mut sum = i16x8::splat(0);
        for i in (0..mid_data.len()).step_by(8) {
            sum += i16x8::from_slice(&mid_data[i..]);
        }

        sum.reduce_sum()
    }
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
