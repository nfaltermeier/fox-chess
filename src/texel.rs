use std::fs;
use std::io::Write;
use std::ops::Add;
use std::ops::Range;
use std::simd::i16x8;
use std::simd::num::SimdInt;
use std::time::Duration;
use std::time::Instant;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    time::{SystemTime, UNIX_EPOCH},
};

#[allow(internal_features)]
use std::intrinsics::fadd_algebraic;

use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;
use rayon::prelude::*;

use crate::board::PIECE_PAWN;
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

pub struct TaperedFeature {
    pub weight: i16,
    pub idx: u16,
    pub taper_amount: i16,
    pub max_amount: i16,
}

impl Default for TaperedFeature {
    fn default() -> Self {
        Self { weight: 0, idx: 0, taper_amount: 0, max_amount: 1 }
    }
}

#[derive(Default)]
pub struct FeatureData {
    pub midgame_psqt_white: [u16; 16],
    pub midgame_psqt_black: [u16; 16],
    pub endgame_psqt_white: [u16; 16],
    pub endgame_psqt_black: [u16; 16],
    pub game_stage: i16,
    pub misc_features: [(i8, u16); MAX_MISC_FEATURES],
    pub pawn_shield: TaperedFeature,
}

pub struct PositionFeatures {
    pub features: FeatureData,
    pub result: f64,
}

pub const EVAL_PARAM_COUNT: usize = 800;
pub type EvalParams = [i16; EVAL_PARAM_COUNT];
pub type EvalGradient = [f64; EVAL_PARAM_COUNT];

#[repr(u16)]
#[derive(Clone, Copy)]
pub enum FeatureIndex {
    MidgamePawn = 0,
    MidgameKnight = 64 * 1,
    MidgameBishop = 64 * 2,
    MidgameRook = 64 * 3,
    MidgameQueen = 64 * 4,
    MidgameKing = 64 * 5,
    EndgamePawn = 64 * 6,
    EndgameKnight = 64 * 7,
    EndgameBishop = 64 * 8,
    EndgameRook = 64 * 9,
    EndgameQueen = 64 * 10,
    EndgameKing = 64 * 11,
    /// Index of the None piece
    PieceValues = 64 * 12,
    DoubledPawns = 782,
    PassedPawns = 784,
    RookOpenFile = 786,
    RookHalfOpenFile = 788,
    BishopPair = 790,
    PawnShield = 792,
    ConnectedPawns = 794,
    // Not included in tuning because I think this is more of a search issue. Quiet positions will usually not have pawns threaten pieces.
    PawnsThreatenPieces = 796,
    IsolatedPawns = 798,
}

static FEATURE_SETS: [FeatureSet; 21] = [
    FeatureSet::new("MidgamePawn", FeatureIndex::MidgamePawn, FeatureIndex::MidgameKnight),
    FeatureSet::new("MidgameKnight", FeatureIndex::MidgameKnight, FeatureIndex::MidgameBishop),
    FeatureSet::new("MidgameBishop", FeatureIndex::MidgameBishop, FeatureIndex::MidgameRook),
    FeatureSet::new("MidgameRook", FeatureIndex::MidgameRook, FeatureIndex::MidgameQueen),
    FeatureSet::new("MidgameQueen", FeatureIndex::MidgameQueen, FeatureIndex::MidgameKing),
    FeatureSet::new("MidgameKing", FeatureIndex::MidgameKing, FeatureIndex::EndgamePawn),
    FeatureSet::new("EndgamePawn", FeatureIndex::EndgamePawn, FeatureIndex::EndgameKnight),
    FeatureSet::new("EndgameKnight", FeatureIndex::EndgameKnight, FeatureIndex::EndgameBishop),
    FeatureSet::new("EndgameBishop", FeatureIndex::EndgameBishop, FeatureIndex::EndgameRook),
    FeatureSet::new("EndgameRook", FeatureIndex::EndgameRook, FeatureIndex::EndgameQueen),
    FeatureSet::new("EndgameQueen", FeatureIndex::EndgameQueen, FeatureIndex::EndgameKing),
    FeatureSet::new("EndgameKing", FeatureIndex::EndgameKing, FeatureIndex::PieceValues),
    FeatureSet::new("PieceValues", FeatureIndex::PieceValues, FeatureIndex::DoubledPawns),
    FeatureSet::new("DoubledPawns", FeatureIndex::DoubledPawns, FeatureIndex::PassedPawns),
    FeatureSet::new("PassedPawns", FeatureIndex::PassedPawns, FeatureIndex::RookOpenFile),
    FeatureSet::new("RookOpenFile", FeatureIndex::RookOpenFile, FeatureIndex::RookHalfOpenFile),
    FeatureSet::new("RookHalfOpenFile", FeatureIndex::RookHalfOpenFile, FeatureIndex::BishopPair),
    FeatureSet::new("BishopPair", FeatureIndex::BishopPair, FeatureIndex::PawnShield),
    FeatureSet::new("PawnShield", FeatureIndex::PawnShield, FeatureIndex::ConnectedPawns),
    FeatureSet::new("ConnectedPawns", FeatureIndex::ConnectedPawns, FeatureIndex::PawnsThreatenPieces),
    FeatureSet::new_mixed("IsolatedPawns", FeatureIndex::IsolatedPawns, EVAL_PARAM_COUNT),
];

#[rustfmt::skip]
pub static DEFAULT_PARAMS: EvalParams = [
        0,0,0,0,0,0,0,0,
        167,148,137,130,86,59,16,55,
        41,52,51,47,52,67,74,33,
        8,3,7,10,26,24,16,4,
        -7,-8,-3,5,3,10,2,-10,
        -12,-18,-14,-15,-7,-2,8,-8,
        -10,-13,-13,-31,-17,16,27,-14,
        0,0,0,0,0,0,0,0,
        -124,-21,-7,-15,24,-46,-39,-118,
        -10,4,22,40,36,38,-33,-10,
        6,27,43,64,80,81,40,6,
        1,14,41,62,37,64,28,32,
        -13,7,28,17,31,24,32,-1,
        -31,-9,3,16,27,9,12,-22,
        -49,-23,-14,-4,-6,0,-15,-11,
        -81,-40,-37,-25,-26,-18,-41,-66,
        -13,-29,-36,-18,-37,-57,-14,20,
        -7,3,6,-9,9,4,-9,-19,
        -1,17,27,35,37,66,39,27,
        1,3,23,49,32,30,-1,-2,
        -13,9,11,28,21,4,1,-4,
        -18,5,7,8,7,4,5,1,
        -13,-8,4,-13,-4,0,12,-15,
        -37,-26,-31,-29,-23,-36,-23,-27,
        50,47,43,38,35,36,47,40,
        25,27,47,56,47,68,54,53,
        11,23,29,36,44,65,50,23,
        -7,-4,11,15,15,24,14,-4,
        -23,-17,-10,-3,-5,-5,1,-21,
        -30,-24,-21,-23,-18,-18,2,-22,
        -34,-27,-16,-20,-18,-3,-17,-49,
        -13,-14,-10,-8,-5,-3,-24,-21,
        4,21,28,39,67,78,71,59,
        -7,-11,24,30,36,72,46,64,
        -2,6,20,35,54,105,87,59,
        -8,1,13,23,34,40,43,35,
        -7,2,9,16,20,17,23,17,
        -7,-1,3,4,4,10,19,-1,
        -17,-2,4,5,8,1,-16,-37,
        -11,-15,-9,-1,-8,-38,-49,-27,
        -28,52,32,-40,65,-14,51,-46,
        13,-2,54,-15,-38,37,-7,17,
        -28,8,0,-4,-27,-29,-38,-40,
        27,44,13,10,18,26,42,27,
        16,40,36,32,30,25,6,-7,
        3,21,16,15,15,8,7,-15,
        13,1,9,-14,-6,-4,26,24,
        -40,12,0,-44,13,-34,33,21,
        0,0,0,0,0,0,0,0,
        151,163,164,110,150,168,223,175,
        91,80,53,31,10,19,34,43,
        40,25,10,-21,-22,-18,-1,-4,
        16,14,-12,-28,-23,-14,-10,-19,
        10,2,-7,-15,-15,-15,-20,-21,
        26,15,5,23,11,-9,-15,-5,
        0,0,0,0,0,0,0,0,
        19,1,5,10,-3,25,-5,-47,
        -13,10,7,14,4,-19,9,-28,
        -11,3,19,13,-12,-22,-9,-29,
        -2,5,27,10,29,-5,11,-26,
        -8,17,28,37,26,16,-15,-14,
        -26,19,21,26,11,3,-15,-20,
        -40,-15,7,14,4,-10,-22,-59,
        -38,-57,-25,-9,-24,-39,-55,-81,
        28,47,42,35,32,35,17,-12,
        11,27,29,28,21,15,21,-15,
        15,20,14,10,1,4,14,-5,
        19,29,17,17,22,8,33,2,
        15,28,32,25,32,30,19,5,
        -7,13,31,26,39,14,-6,-20,
        -18,-1,1,22,8,-4,-28,-51,
        -10,13,-14,0,-4,-11,-6,4,
        45,57,64,65,70,65,62,64,
        70,72,64,55,62,41,47,44,
        72,65,63,58,48,39,42,57,
        71,73,67,62,51,50,49,60,
        66,69,67,62,53,55,48,53,
        42,44,46,40,40,35,21,34,
        31,25,31,31,24,10,22,32,
        44,41,55,54,36,32,57,40,
        95,114,127,142,99,81,73,83,
        92,134,142,147,151,88,103,49,
        67,104,117,132,116,58,45,25,
        65,113,134,140,134,130,102,82,
        71,105,104,121,114,126,98,86,
        58,63,103,88,88,92,86,59,
        53,34,61,50,33,-2,15,50,
        49,46,50,28,49,8,10,-5,
        -69,-36,-2,23,-11,25,-2,-64,
        -9,31,1,43,50,27,49,4,
        19,38,48,51,59,73,78,48,
        -6,15,34,39,40,39,27,5,
        -35,-10,10,19,24,21,11,-12,
        -47,-22,-3,9,12,10,-4,-16,
        -51,-18,-10,0,0,0,-19,-46,
        -3,-54,-24,-14,-47,-16,-64,-87,
        0,0,79,121,293,293,313,313,
        449,516,921,994,20000,20000,-10,-24,
        7,15,37,2,19,36,28,88,
        -8,-8,8,4,56,34,-11,-6,
    ];

pub fn load_positions(filename: &str) -> Vec<TexelPosition> {
    let positions_to_use = 9_000_000;
    let mut result = Vec::with_capacity(positions_to_use);

    // Based on a blank line being inserted between each game by pgn-extract
    let file = File::open(filename).unwrap();
    let games_count = BufReader::new(file).lines().filter(|l| l.as_ref().unwrap().is_empty()).count() + 1;

    let positions_per_game = positions_to_use / games_count;
    // Find out how many positions we will be short because of integer rounding
    let extra_positions = positions_to_use - positions_per_game * games_count;
    let mut extra_positions_left = extra_positions;

    let mut current_game_positions = Vec::with_capacity(200);

    let mut rand = StdRng::seed_from_u64(0x88d885d4bb51ffc2);

    let file = File::open(filename).unwrap();
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();

        if !line.is_empty() {
            let c2_index = line.find("c2");
            // The first positions are when it left book but they lack a comment
            if let Some(c2_index) = c2_index {
                let c2 = &line[c2_index..];

                // Skip starting positions and book moves.
                // Also If forced mate was found or evaluation was skipped because only one move was possible (which may indicate the player is being mated) then skip
                if c2.is_empty() || c2.contains("book") || c2.contains("M") || c2.contains("/1;") || c2.contains("/1 ") {
                    continue;
                }
            }

            current_game_positions.push(line);
            continue;
        }

        let positions_to_take = if extra_positions_left > 0 && rand.gen_ratio(extra_positions as u32, games_count as u32) {
            extra_positions_left -= 1;
            positions_per_game + 1
        } else {
            positions_per_game
        };

        for i in 0..positions_to_take {
            if current_game_positions.len() == 0 {
                extra_positions_left += positions_to_take - i + 1;
                break;
            }

            let line = current_game_positions.swap_remove(rand.gen_range(0..current_game_positions.len()));

            let c0_index = line.find("c0");
            if c0_index.is_none() {
                panic!("Could not find c0, possibly a malformed line: {line}");
            }
            let c0_index = c0_index.unwrap();

            let fen = &line[..c0_index];

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

        current_game_positions.clear();
    }

    println!("Loaded {games_count} games and targetted {positions_per_game} positions per game");

    result
}

fn sigmoid(eval: f64, scaling_constant: f64) -> f64 {
    let exp = -eval * scaling_constant / 400.0;
    1.0 / (1.0 + 10.0_f64.powf(exp))
}

#[derive(Clone)]
struct Improvement {
    pub improvement: f64,
    pub step_size: f64,
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

    // perturb(&mut params);

    let quiet = true;
    if quiet {
        println!("Running in quiet mode, skipping some printouts and saves");
    }

    let scaling_constant = 1.06;
    let mut step_size;
    // step_size is scaled by this when Armijo–Goldstein condition is not filfilled. Should be within (0, 1).
    let tau = 0.95;

    let mut iterations = 0;
    let mut changed_since_step_size_reset = false;
    let mut step_size_resets = 0;
    let mut total_param_changes = 0;
    let mut starting_error = None;
    let mut feature_set_loops = 0;
    let mut failures: usize = 0;
    let mut last_disk_save = Instant::now();

    // The goal for how much to improve at each descent step
    for base_c in [0.1] {
        loop {
            let mut any_changed_in_feature_set = false;
            for feature_set in &FEATURE_SETS {
                let feature_set_range = feature_set.lower..feature_set.upper;

                {
                    let mut any_can_change = false;
                    for i in feature_set_range.clone() {
                        if change_param_at_index(i) {
                            any_can_change = true;
                            break;
                        }
                    }

                    if !any_can_change {
                        continue;
                    }
                }

                let (c, default_step_size) = if feature_set_range.end - feature_set_range.start > 100 { (base_c, 10_000_000.0) } else { (3.0 * base_c, 100_000_000.0) };
                step_size = default_step_size;
                
                loop {
                    let features = qsearch_for_features(&mut nonquiet_positions, &params);

                    let base_error = find_error_for_features(&features, &params, scaling_constant);
                    println!("[{}] Starting new loop, new error is {base_error:.8}", humantime::format_rfc3339(SystemTime::now()));
                    if starting_error.is_none() {
                        starting_error = Some(base_error);
                    }

                    let gradient = eval_gradient(&features, &mut params, scaling_constant, feature_set_range.clone(), quiet);
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
                    let mut found_improvement = false;
                    let mut best_improvement: Option<Improvement> = None;
                    let mut lowest_step_size_improvement = None;
                    let mut search_up = 0;
                    let mut search_down = 0;
                    let mut final_loop = false;
                    loop {
                        updated_params = params;
                        let mut changes = Vec::with_capacity(EVAL_PARAM_COUNT);
                        gradient.par_iter().map(|v| (-v * step_size).round() as i16).collect_into_vec(&mut changes);
                        biggest_change = changes.iter().map(|v| v.abs()).max().unwrap();

                        // if biggest_change >= 100 {
                        //     panic!("Biggest change {biggest_change} could cause an overflow")
                        // } else
                        if biggest_change == 0 {
                            println!("[{}] Biggest change {biggest_change} for step size {step_size}", humantime::format_rfc3339(SystemTime::now()));

                            if found_improvement {
                                final_loop = true;
                                step_size = best_improvement.as_ref().unwrap().step_size;
                                continue;
                            } else if changed_since_step_size_reset {
                                // Maybe the bigger derivatives have settled down now,
                                // retry from the start to give the smaller derivatives a chance to change.
                                // Params have not changed so reuse the gradient.
                                step_size = default_step_size;
                                changed_since_step_size_reset = false;
                                step_size_resets += 1;
                                println!("[{}] ##### Resetting step size for the {step_size_resets}{} time #####", humantime::format_rfc3339(SystemTime::now()), get_ordinal_suffix(step_size_resets));

                                continue;
                            } else {
                                break;
                            }
                        }

                        updated_params.par_iter_mut().zip(&changes).for_each(|(param, change)| *param += change);

                        // I think searching for error should be more accurate to the true error than reusing the found quiet position features but it is insanely slow, maybe because of cache stuff?
                        new_error = find_error_for_features(&features, &updated_params, scaling_constant);

                        if base_error - new_error >= step_size * t {
                            println!("[{}] Biggest change {biggest_change} for step size {step_size}", humantime::format_rfc3339(SystemTime::now()));
                            println!("[{}] Armijo-Goldstein condition passed: {} >= {}", humantime::format_rfc3339(SystemTime::now()), base_error - new_error, step_size * t);

                            if final_loop {
                                changed_params = changes.iter().filter(|v| **v != 0).count();
                                total_param_changes += changed_params;
                                changed_since_step_size_reset = true;
                                any_changed_in_feature_set = true;
                                break;
                            }

                            let improvement = base_error - new_error;
                            if !found_improvement {
                                /*
                                * Testing step size is way *way* faster than recalculating a gradient so once a step_size that improves the error is found,
                                * do some searching around to find if a nearby step size improves it even more.
                                */
                                best_improvement = Some(Improvement {
                                    step_size,
                                    improvement,
                                });
                                lowest_step_size_improvement = best_improvement.clone();
                                found_improvement = true;
                                search_up = 6;
                                search_down = 6;
                            } else {
                                if improvement > best_improvement.as_ref().unwrap().improvement {
                                    best_improvement = Some(Improvement {
                                        step_size,
                                        improvement,
                                    });

                                    // Let it keep searching for an improvement
                                    if search_up > 1 {
                                        search_up += 1;
                                    } else {
                                        search_down += 1;
                                    }
                                }
                            }
                        } else {
                            failures += 1;
                            if !quiet || failures % 5 == 0 {
                                println!("[{}] Biggest change {biggest_change} for step size {step_size}", humantime::format_rfc3339(SystemTime::now()));
                                println!("[{}] Armijo-Goldstein condition failed: {} < {}", humantime::format_rfc3339(SystemTime::now()), base_error - new_error, step_size * t);
                            }
                            step_size *= tau;
                        }

                        if found_improvement {
                            if search_up > 1 {
                                step_size /= tau;
                                search_up -= 1;

                                if search_up == 0 {
                                    step_size = lowest_step_size_improvement.as_ref().unwrap().step_size;
                                }
                            } else if search_down > 1 {
                                step_size *= tau;
                                search_down -= 1;
                            } else {
                                final_loop = true;
                                step_size = best_improvement.as_ref().unwrap().step_size;
                            }
                        }
                    }

                    // To find the appropriate step size we descend the gradient, so we just use that value as our next value
                    params = updated_params;

                    if biggest_change == 0 {
                        new_error = base_error;
                    }

                    iterations += 1;
                    let will_save = !quiet || last_disk_save.elapsed() > Duration::from_secs(30);
                    println!(
                        "[{}] {}error: {new_error:.8}, iterations: {iterations}, step size: {step_size:.1}, biggest change: {biggest_change}, changed {changed_params} params, total param changes {total_param_changes}, total error reduction {:.8}, step_size_resets: {step_size_resets}, feature_set: {}, feature_set_loops: {feature_set_loops}, base_c: {base_c}",
                        humantime::format_rfc3339(SystemTime::now()),
                        if will_save { "Saving, " } else { "" },
                        starting_error.unwrap() - new_error,
                        feature_set.name,
                    );

                    if will_save {
                        save_params(&params);
                        last_disk_save = Instant::now();
                    }

                    if biggest_change == 0 {
                        break;
                    }
                }
            }

            if !any_changed_in_feature_set {
                break;
            }

            feature_set_loops += 1;
        }
    }

    println!("Regression done");
    save_params(&params);
}

pub fn change_param_at_index(i: usize) -> bool {
    if i < 8
        // midgame pawns on last row
        || (i >= 56 && i < 64)
        // endgame pawns on first row
        || (i >= 6 * 64 && i < 8 + 6 * 64)
        // endgame pawns on last row
        || (i >= 56 + 6 * 64 && i < 64 + 6 * 64)
        // None piece centipawn value midgame
        || i == FeatureIndex::PieceValues as usize
        // None piece centipawn value endgame
        || i == FeatureIndex::PieceValues as usize + 1
        // King centipawn value midgame
        || i == FeatureIndex::PieceValues as usize + PIECE_KING as usize * 2
        // King centipawn value endgame
        || i == FeatureIndex::PieceValues as usize + PIECE_KING as usize * 2 + 1
        // Not included in tuning because I think this is more of a search issue. Quiet positions will usually not have pawns threaten pieces.
        || value_is_between(i, FeatureIndex::ConnectedPawns, FeatureIndex::PawnsThreatenPieces)
    {
        return false;
    }

    return !value_is_between(i, FeatureIndex::PieceValues, FeatureIndex::DoubledPawns) || is_piece_type(i, PIECE_PAWN);
}

fn search_error_for_params(positions: &mut Vec<TexelPosition>, params: &EvalParams, scaling_constant: f64) -> f64 {
    let mut errors = Vec::with_capacity(positions.len());
    positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r).0;

            if result == -i16::MAX {
                0.0
            } else {
                let eval = (result * if p.board.white_to_move { 1 } else { -1 }) as f64;
                let val_sqrt = p.result - sigmoid(eval, scaling_constant);
                val_sqrt * val_sqrt
            }
        })
        .collect_into_vec(&mut errors);

    sum_orlp(&errors[..]) / positions.len() as f64
}

fn qsearch_for_features(positions: &mut Vec<TexelPosition>, params: &EvalParams) -> Vec<PositionFeatures> {
    let mut result = Vec::with_capacity(positions.len());
    positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r);

            // if all positions were in check for stm
            if result.0 == -i16::MAX {
                PositionFeatures {
                    features: FeatureData::default(),
                    result: 0.5
                }
            } else {
                PositionFeatures {
                    features: result.1.get_eval_features(),
                    result: p.result,
                }
            }
        })
        .collect_into_vec(&mut result);

    result
}

fn find_error_for_features(
    features: &Vec<PositionFeatures>,
    params: &EvalParams,
    scaling_constant: f64,
) -> f64 {
    let mut errors = Vec::with_capacity(features.len());
    features
        .par_iter()
        .map(|p| {
            let val_sqrt = p.result - sigmoid(p.features.evaluate(params) as f64, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect_into_vec(&mut errors);

    let sum = sum_orlp(&errors[..]);
    sum / errors.len() as f64
}

/// params should be unchanged when this method returns
fn eval_gradient(features: &Vec<PositionFeatures>, params: &mut EvalParams, scaling_constant: f64, feature_set_range: Range<usize>, quiet: bool) -> Box<EvalGradient> {
    let mut result = Box::new([0f64; EVAL_PARAM_COUNT]);

    for i in feature_set_range {
        // midgame pawns on first row
        if !change_param_at_index(i) {
            continue;
        }

        params[i] += 1;

        let positive_error = find_error_for_features(features, params, scaling_constant);

        params[i] -= 2;

        let negative_error = find_error_for_features(features, params, scaling_constant);

        params[i] += 1;

        // Approximate the derivative with symmetrical difference quotient numerical differentiation
        result[i] = (positive_error - negative_error) / 2.0;

        if !quiet && i % 100 == 0 {
            println!("[{}] Calculated derivative for {i} elements of gradient", humantime::format_rfc3339(SystemTime::now()))
        }
    }

    result
}

/// Finds the dot product of the gradient and the search direction p
fn calc_m(gradient: &Box<EvalGradient>) -> f64 {
    // p is the negative gradient so first square everything and make them negative
    let mut negative_squares = Vec::with_capacity(gradient.len());
    gradient.par_iter().map(|v| -v * v).collect_into_vec(&mut negative_squares);
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
        let position_score_midgame = FeatureData::sum_psqt_for_a_phase(&self.midgame_psqt_white, &self.midgame_psqt_black, &self.misc_features, params, false);

        let position_score_endgame = FeatureData::sum_psqt_for_a_phase(&self.endgame_psqt_white, &self.endgame_psqt_black, &self.misc_features, params, true);

        let position_score_final = (((position_score_midgame as i32 * self.game_stage as i32)
            + (position_score_endgame as i32 * (MIN_GAME_STAGE_FULLY_MIDGAME as i32 - self.game_stage as i32)))
            / MIN_GAME_STAGE_FULLY_MIDGAME as i32) as i16;

        let pawn_shield = (self.pawn_shield.taper_amount * self.pawn_shield.weight * params[self.pawn_shield.idx as usize]) / self.pawn_shield.max_amount;

        position_score_final + pawn_shield
    }
    
    pub fn sum_psqt_for_a_phase(white: &[u16; 16], black: &[u16; 16], misc_features: &[(i8, u16); MAX_MISC_FEATURES], params: &EvalParams, endgame: bool) -> i16 {
        let mut mid_data = [0; 32 + MAX_MISC_FEATURES];
        for (a_i, p_i) in white.iter().enumerate() {
            mid_data[a_i] = params[*p_i as usize];
        }
        for (a_i, p_i) in black.iter().enumerate() {
            mid_data[a_i + 16] = -params[*p_i as usize];
        }

        for (a_i, feature) in misc_features.iter().enumerate() {
            let (w, i) = feature;
            mid_data[a_i + 32] = *w as i16 * params[*i as usize + if endgame { 1 } else { 0 }];
        }

        let mut sum = i16x8::splat(0);
        for i in (0..mid_data.len()).step_by(8) {
            sum += i16x8::from_slice(&mid_data[i..]);
        }

        sum.reduce_sum()
    }
}

fn perturb(params: &mut EvalParams) {
    let mut rand = StdRng::from_entropy();
    for i in 0..params.len() {
        if change_param_at_index(i) {
            params[i] += rand.gen_range(-50..=50);
        }
    }
}

impl Add<u16> for FeatureIndex {
    type Output = u16;

    fn add(self, rhs: u16) -> Self::Output {
        self as u16 + rhs
    }
}

pub struct FeatureSet<'a> {
    pub name: &'a str,
    /// inclusive lower bound
    pub lower: usize,
    /// exclusive upper bound
    pub upper: usize,
}

impl<'a> FeatureSet<'a> {
    pub const fn new(name: &'a str, lower: FeatureIndex, upper: FeatureIndex) -> Self {
        Self {
            name,
            lower: lower as usize,
            upper: upper as usize,
        }
    }

    pub const fn new_usize(name: &'a str, lower: usize, upper: usize) -> Self {
        Self {
            name,
            lower,
            upper,
        }
    }

    pub const fn new_mixed(name: &'a str, lower: FeatureIndex, upper: usize) -> Self {
        Self {
            name,
            lower: lower as usize,
            upper,
        }
    }

    pub const fn new_single(name: &'a str, index: FeatureIndex) -> Self {
        Self {
            name,
            lower: index as usize,
            upper: index as usize + 1,
        }
    }
}

fn value_is_between(i: usize, lower: FeatureIndex, upper: FeatureIndex) -> bool {
    i >= lower as usize && i < upper as usize
}

fn is_piece_type(i: usize, piece_type: u8) -> bool {
    // Midgame piece value
    i == FeatureIndex::PieceValues as usize + piece_type as usize * 2
    // Endgame piece value
    || i == FeatureIndex::PieceValues as usize + piece_type as usize * 2 + 1
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
        3 => "rd",
        _ => "th",
    }
}
