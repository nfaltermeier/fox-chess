use std::fs;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Write;
use std::iter::repeat_n;
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

use rand::seq::SliceRandom;
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

pub const MAX_MISC_FEATURES: usize = 32;

#[derive(Default)]
pub struct FeatureData {
    pub midgame_psqt_white: [u16; 16],
    pub midgame_psqt_black: [u16; 16],
    pub endgame_psqt_white: [u16; 16],
    pub endgame_psqt_black: [u16; 16],
    pub game_stage: i16,
    pub misc_features: [(i8, u16); MAX_MISC_FEATURES],
    pub pawn_shield: TaperedFeature,
    pub attack_unit_piece_counts: [[u8; 4]; 2],
    pub endgame_bonus: i16,
}

pub struct PositionFeatures {
    pub features: FeatureData,
    pub result: f64,
}

enum PositionsType<'a> {
    Quiet(&'a [PositionFeatures]),
    Nonquiet(&'a mut [TexelPosition]),
}

pub const EVAL_PARAM_COUNT: usize = 910;
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
    RookMobility = 800,
    BishopMobility = 830,
    KnightMobility = 858,
    QueenMobility = 876,
    AttackUnitsCentiMultiplier = 904,
    AttackUnitsXOffset = 905,
    AttackUnitsPieceValues = 906,
}

static FEATURE_SETS: [FeatureSet; 32] = [
    FeatureSet::new_usize("MidgamePawnClose", 24, FeatureIndex::MidgameKnight as usize),
    FeatureSet::new_mixed("MidgamePawnFar", FeatureIndex::MidgamePawn, 24),
    FeatureSet::new_usize("MidgameKnightClose", FeatureIndex::MidgameKnight as usize + 16, FeatureIndex::MidgameBishop as usize),
    FeatureSet::new_mixed("MidgameKnightFar", FeatureIndex::MidgameKnight, FeatureIndex::MidgameKnight as usize + 16),
    FeatureSet::new_usize("MidgameBishopClose", FeatureIndex::MidgameBishop as usize + 16, FeatureIndex::MidgameRook as usize),
    FeatureSet::new_mixed("MidgameBishopFar", FeatureIndex::MidgameBishop, FeatureIndex::MidgameBishop as usize + 16),
    FeatureSet::new("MidgameRook", FeatureIndex::MidgameRook, FeatureIndex::MidgameQueen),
    FeatureSet::new("MidgameQueen", FeatureIndex::MidgameQueen, FeatureIndex::MidgameKing),
    FeatureSet::new_usize("MidgameKingClose", FeatureIndex::MidgameKing as usize + 48, FeatureIndex::EndgamePawn as usize),
    FeatureSet::new_mixed("MidgameKingFar", FeatureIndex::MidgameKing, FeatureIndex::EndgamePawn as usize + 48),
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
    FeatureSet::new_single("BishopPair", FeatureIndex::BishopPair),
    FeatureSet::new("PawnShield", FeatureIndex::PawnShield, FeatureIndex::ConnectedPawns),
    FeatureSet::new("ConnectedPawns", FeatureIndex::ConnectedPawns, FeatureIndex::PawnsThreatenPieces),
    FeatureSet::new("IsolatedPawns", FeatureIndex::IsolatedPawns, FeatureIndex::RookMobility),
    FeatureSet::new("RookMobility", FeatureIndex::RookMobility, FeatureIndex::BishopMobility),
    FeatureSet::new("BishopMobility", FeatureIndex::BishopMobility, FeatureIndex::KnightMobility),
    FeatureSet::new("KnightMobility", FeatureIndex::KnightMobility, FeatureIndex::QueenMobility),
    FeatureSet::new("QueenMobility", FeatureIndex::QueenMobility, FeatureIndex::AttackUnitsCentiMultiplier),
    FeatureSet::new_single("AttackUnitsCentiMultiplier", FeatureIndex::AttackUnitsCentiMultiplier),
    FeatureSet::new_single("AttackUnitsXOffset", FeatureIndex::AttackUnitsXOffset),
    FeatureSet::new_mixed("AttackUnitsPieceValues", FeatureIndex::AttackUnitsPieceValues, EVAL_PARAM_COUNT),
];

#[rustfmt::skip]
pub static DEFAULT_PARAMS: EvalParams = [
        0,0,0,0,0,0,0,0,
        188,160,145,137,88,41,-28,31,
        46,49,51,47,54,68,60,32,
        7,4,2,7,23,22,8,3,
        -4,-8,-2,5,4,14,1,-12,
        -12,-14,-16,-14,-7,-2,7,-9,
        -7,-10,-17,-18,-13,15,25,-14,
        0,0,0,0,0,0,0,0,
        -114,-24,-24,-10,19,-20,-23,-146,
        -11,-1,24,36,28,35,-16,9,
        -6,12,32,53,77,76,42,12,
        1,6,31,48,26,47,25,36,
        -5,3,21,14,27,24,37,6,
        -29,-10,-4,10,23,6,7,-12,
        -40,-22,-13,4,7,-1,-9,-4,
        -81,-23,-32,-14,-12,-10,-12,-66,
        -11,-25,-15,-15,-3,-51,21,8,
        -19,-5,-2,-2,8,-2,-27,-25,
        -7,11,21,25,33,64,40,27,
        -10,-1,16,42,23,26,-4,-4,
        -1,-1,6,28,20,-6,-1,4,
        -14,8,8,6,7,6,7,0,
        -7,-6,8,-8,1,6,14,-5,
        -36,-7,-17,-20,-11,-18,-23,-26,
        39,42,38,37,35,39,47,40,
        21,21,37,42,46,66,53,53,
        7,21,24,31,48,66,53,30,
        -7,-3,12,17,19,25,24,6,
        -20,-20,-11,-1,-2,-4,7,-19,
        -30,-20,-24,-19,-11,-12,3,-24,
        -38,-30,-15,-12,-13,-3,-13,-50,
        -15,-14,-7,-5,-1,1,-21,-23,
        5,27,27,39,56,79,77,67,
        -7,-8,21,29,46,67,52,67,
        1,8,20,30,62,98,92,67,
        2,6,15,24,39,38,47,34,
        -1,4,13,18,19,21,29,20,
        -2,5,8,11,12,19,23,13,
        -7,2,10,17,18,12,-5,-14,
        -2,-3,2,12,5,-29,-51,-27,
        -28,52,32,-40,65,-14,51,-46,
        13,-2,54,-15,-38,37,-7,17,
        -28,8,0,-4,-27,-28,-36,-40,
        27,43,13,10,17,27,42,27,
        16,39,36,34,29,24,11,-11,
        3,17,16,16,12,10,2,-15,
        -2,-8,11,-6,-6,-1,24,18,
        -46,10,6,-40,9,-29,26,12,
        0,0,0,0,0,0,0,0,
        76,93,92,42,73,118,173,127,
        85,80,49,22,7,18,40,42,
        48,38,22,-4,-7,-2,14,8,
        27,26,3,-15,-11,0,9,-1,
        21,14,10,-1,1,5,-3,-2,
        34,22,18,-1,16,7,-2,8,
        0,0,0,0,0,0,0,0,
        54,41,38,42,17,36,28,31,
        14,40,22,40,21,12,43,-16,
        29,30,57,40,17,18,14,5,
        21,30,55,42,58,33,28,4,
        24,31,59,63,46,44,10,13,
        -2,28,47,47,29,21,1,1,
        -4,19,12,16,9,-6,10,-38,
        -7,-15,14,15,-3,-22,-28,-8,
        44,65,49,45,28,45,22,16,
        48,43,43,37,33,36,44,20,
        33,38,27,19,19,15,24,9,
        30,42,30,32,39,22,51,27,
        28,42,47,32,31,44,34,17,
        12,28,40,37,48,24,8,3,
        10,13,11,34,13,10,-6,-21,
        23,14,9,21,10,5,10,16,
        79,89,97,94,101,92,86,88,
        107,108,104,99,93,77,84,79,
        109,102,102,97,83,73,79,87,
        106,107,102,97,90,85,85,91,
        101,107,105,93,88,90,79,88,
        81,79,86,75,67,63,53,70,
        70,71,68,56,52,42,55,69,
        64,58,69,63,42,42,64,66,
        145,141,167,171,166,112,112,116,
        142,186,202,193,183,161,149,103,
        113,168,183,201,168,115,102,99,
        125,150,176,188,170,185,165,147,
        112,150,150,164,156,163,136,153,
        86,116,139,113,132,123,113,85,
        88,86,92,66,64,33,61,61,
        62,40,61,21,53,9,65,20,
        -78,-31,1,30,-6,39,14,-114,
        -17,34,16,45,52,38,60,-1,
        20,49,50,55,64,79,82,50,
        -8,20,41,41,40,42,31,6,
        -33,-6,10,18,22,19,8,-11,
        -47,-18,-2,8,10,5,-7,-19,
        -38,-10,-14,-8,-7,-9,-28,-54,
        -21,-50,-39,-20,-51,-25,-71,-89,
        0,0,86,110,334,295,353,322,
        508,535,1042,997,20000,20000,-14,-23,
        7,19,30,-6,15,24,32,88,
        -9,-8,6,-2,55,34,-11,-14,
        -17,0,-12,-1,-7,-1,-4,-2,
        -3,4,3,5,5,12,8,7,
        11,13,14,15,17,17,18,19,
        24,14,20,14,27,-3,-28,0,
        -19,-12,-11,-17,-6,-6,-1,2,
        5,8,9,14,11,19,16,13,
        18,15,18,10,16,10,12,12,
        13,12,-41,0,-19,1,-7,2,
        -1,5,6,11,12,17,17,13,
        21,1,12,-16,7,1,2,2,
        2,3,4,4,8,5,13,6,
        17,9,22,14,30,11,37,10,
        39,10,26,12,13,13,14,14,
        142,35,6,36,49,57,
    ];

pub fn load_positions(filename: &str) -> Vec<TexelPosition> {
    let positions_to_use = 9_000_000;
    let mut result = Vec::with_capacity(positions_to_use);

    // Based on a blank line being inserted between each game by pgn-extract
    let file = File::open(filename).unwrap();
    let mut games_count = BufReader::new(file).lines().filter(|l| l.as_ref().unwrap().is_empty()).count() + 1;

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

        let mut first_position_from_game = true;
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

            if first_position_from_game {
                // Check for draws in KNBvK endgames and skip them.
                // Most of my current training data was generated when it could not solve these endgames reliably,
                // so they do not reflect the current state of the engine at all.
                if match_result_value == 0.5 && current_game_positions.len() > 1 {
                    // Assuming the positions have not been shuffled
                    let second_last_position = &current_game_positions[current_game_positions.len() - 1];
                    let pieces = &second_last_position[0..second_last_position.find(' ').unwrap()];
                    let lowercase_pieces = pieces.to_ascii_lowercase();
                    if !lowercase_pieces.contains('p')
                        && !lowercase_pieces.contains('r')
                        && !lowercase_pieces.contains('q') 
                        && lowercase_pieces.contains('b') 
                        && lowercase_pieces.contains('n') 
                        && lowercase_pieces.chars().filter(|c| *c == 'b').count() == 1
                    {
                        if (pieces.contains('b') && pieces.contains('n'))
                            || (pieces.contains('B') && pieces.contains('N'))
                        {
                            games_count -= 1;
                            extra_positions_left += positions_to_take;
                            break;
                        }
                    }
                }

                first_position_from_game = false;
            }

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

pub fn load_preprocessed_positions(filename: &str) -> Vec<TexelPosition> {
    let mut result = Vec::new();

    let file = File::open(filename).unwrap();
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();

        if !line.is_empty() {
            let semicolon_index = line.find(";").unwrap();
            let fen = &line[0..semicolon_index];
            let board = Board::from_fen(fen).unwrap();

            let match_result = &line[semicolon_index + 1..];
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
    }

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
pub fn find_best_params(nonquiet_positions: Option<Vec<TexelPosition>>, quiet_positions: Option<Vec<TexelPosition>>) {
    if !fs::exists("params").unwrap() {
        fs::create_dir("params").unwrap();
    }

    if !fs::exists("gradients").unwrap() {
        fs::create_dir("gradients").unwrap();
    }

    if nonquiet_positions.is_none() && quiet_positions.is_none() {
        panic!("One of nonquiet_positions or quiet_positions must have a value")
    }

    // test starting from zeros
    let mut params = DEFAULT_PARAMS;
    // let mut params = [0; EVAL_PARAM_COUNT];

    // perturb(&mut params);

    let quiet_logging = true;
    if quiet_logging {
        println!("Running in quiet mode, skipping some printouts and saves");
    }

    let scaling_constant = 1.06;
    let mut step_size;
    // step_size is scaled by this when Armijo–Goldstein condition is not filfilled. Should be within (0, 1).
    let tau = 0.95;

    // Pass 0 to disable minibatching
    let minibatches = 5;
    assert_ne!(minibatches, 1);
    let max_change_sets_per_minibatch = 3;
    // Only minibatches will be used until this many loops of minibatches have been run
    let max_minibatched_loops = 3;
    let mut error_reduction_after_minibatched_loop = Vec::new();
    let is_quiet_positions = nonquiet_positions.is_none();
    let mut positions = nonquiet_positions.unwrap_or_else(|| quiet_positions.unwrap());
    let positions_per_minibatch = positions.len() / minibatches.max(1);

    let mut iterations = 0;
    let mut changed_since_step_size_reset = false;
    let mut step_size_resets = 0;
    let mut total_param_changes = 0;
    let mut feature_set_loops = 0;
    let mut failured_param_changes: usize = 0;
    let mut succeeded_param_changes: usize = 0;
    let mut last_disk_save = Instant::now();
    let mut fullbatch_base_error = None;
    let mut rand = if minibatches > 0 {
        StdRng::from_entropy()
    } else {
        // If minibatches are disabled then a reproducible result / fast exit may be desired
        StdRng::seed_from_u64(0x88d885d4bb51ffc2)
    };

    // The goal for how much to improve at each descent step
    for base_c in [0.1] {
        for minibatched_loops_run in 0..=max_minibatched_loops {
            if minibatches >= 2 && minibatched_loops_run < max_minibatched_loops {
                let mut rng = StdRng::from_entropy();
                positions.shuffle(&mut rng);
            }

            let quiet_pos_features = if is_quiet_positions {
                Some(get_features_for_quiet_positions(&positions))
            } else {
                None
            };

            let mut starting_errors = Vec::with_capacity(minibatches + 1);
            // When true, the only work done is setting starting_errors for each batch
            for is_initializing_starting_errors in [true, false] {
                for batch_num in 1..=(minibatches + 1) {
                    let is_fullbatch = batch_num == minibatches + 1;

                    if !is_fullbatch && minibatched_loops_run >= max_minibatched_loops {
                        continue;
                    }

                    if is_initializing_starting_errors && is_fullbatch && let Some(fullbatch_base_error) = fullbatch_base_error {
                        starting_errors.extend(repeat_n(0.0, starting_errors.len() - minibatches));
                        starting_errors.push(fullbatch_base_error);
                        continue;
                    }

                    // batch_data should not have the value it contains changed. The inner value of &&mut cannot be changed,
                    // so batch_data must be mut so instead I end up with &mut &mut where the inner value can be changed.
                    let (mut batch_data, batch_description) = if !is_fullbatch {
                        let start = (batch_num - 1) * positions_per_minibatch;
                        let end = if batch_num != minibatches { batch_num * positions_per_minibatch } else { positions.len() };
                        let batch_data = if is_quiet_positions {
                            PositionsType::Quiet(&quiet_pos_features.as_ref().unwrap()[start..end])
                        } else {
                            PositionsType::Nonquiet(&mut positions[start..end])
                        };
                        (batch_data, format!("Batch {}", batch_num))
                    } else {
                        let batch_data = if is_quiet_positions {
                            PositionsType::Quiet(&quiet_pos_features.as_ref().unwrap()[..])
                        } else {
                            PositionsType::Nonquiet(&mut positions[..])
                        };
                        (batch_data, "Full batch".to_string())
                    };

                    'outer: loop {
                        let mut changes_this_feature_set_loop = 0;
                        let mut starting_error_this_feature_set_loop = None;
                        let mut working_feature_sets = Vec::new();
                        working_feature_sets.extend_from_slice(&FEATURE_SETS);

                        while !working_feature_sets.is_empty() {
                            let feature_set = working_feature_sets.swap_remove(rand.gen_range(0..working_feature_sets.len()));
                            let feature_set_range = feature_set.lower..feature_set.upper;

                            {
                                let mut any_can_change = false;
                                for i in feature_set_range.clone() {
                                    if change_param_at_index(i, is_quiet_positions) {
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
                            let mut change_sets_this_feature = 0;
                            
                            loop {
                                let mut nonquiet_pos_features = None;
                                let features = match &mut batch_data {
                                    PositionsType::Quiet(items) => *items,
                                    PositionsType::Nonquiet(items) => {
                                        nonquiet_pos_features = Some(qsearch_for_features(*items, &params));
                                        &nonquiet_pos_features.unwrap()
                                    },
                                };

                                let base_error =  {
                                    let king_attack_unit_values = generate_king_attack_unit_values(&params);
                                    find_error_for_features(features, &params, scaling_constant, &king_attack_unit_values)
                                };

                                if is_initializing_starting_errors {
                                    starting_errors.push(base_error);
                                    if is_fullbatch && fullbatch_base_error.is_none() {
                                        fullbatch_base_error = Some(base_error);
                                    }
                                    // First establish all of the base error values
                                    break 'outer;
                                } else if minibatches > 0 && is_fullbatch && minibatched_loops_run < max_minibatched_loops {
                                    error_reduction_after_minibatched_loop.push(fullbatch_base_error.unwrap() - base_error);

                                    for (i, v) in error_reduction_after_minibatched_loop.iter().enumerate() {
                                        let msg = format!("[{}] ###### Fullbatch total error progress after {} minibatch loops: {:.8} ######", humantime::format_rfc3339(SystemTime::now()), i + 1, *v);
                                        
                                        for _ in 0..3 {
                                            println!("{msg}");
                                        }
                                    }

                                    break 'outer;
                                }

                                println!("[{}] Starting new loop, new error is {base_error:.8}", humantime::format_rfc3339(SystemTime::now()));
                                if starting_error_this_feature_set_loop.is_none() {
                                    starting_error_this_feature_set_loop = Some(base_error);
                                }

                                let gradient = eval_gradient(features, &mut params, scaling_constant, feature_set_range.clone(), quiet_logging, is_quiet_positions);
                                let biggest_gradient_value = gradient.iter().map(|v| v.abs()).reduce(f64::max).unwrap();
                                let avg_gradient_value = sum_orlp(&*gradient) / gradient.len() as f64;
                                let mut sorted = gradient.clone();
                                sorted.sort_unstable_by(f64::total_cmp);
                                let median_gradient_value = sorted[gradient.len() / 2];
                                println!("[{}] Calculated gradient, biggest gradient value is {biggest_gradient_value}, avg is {avg_gradient_value}, median is {median_gradient_value}", humantime::format_rfc3339(SystemTime::now()));

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
                                            println!("[{}] Resetting step size for the {step_size_resets}{} time", humantime::format_rfc3339(SystemTime::now()), get_ordinal_suffix(step_size_resets));

                                            continue;
                                        } else {
                                            break;
                                        }
                                    }

                                    updated_params.par_iter_mut().zip(&changes).for_each(|(param, change)| *param += change);

                                    let king_attack_unit_values = generate_king_attack_unit_values(&updated_params);
                                    new_error = find_error_for_features(&features, &updated_params, scaling_constant, &king_attack_unit_values);

                                    if base_error - new_error >= step_size * t {
                                        succeeded_param_changes += 1;
                                        if !quiet_logging || is_fullbatch || succeeded_param_changes % 3 == 0 {
                                            println!("[{}] Biggest change {biggest_change} for step size {step_size}", humantime::format_rfc3339(SystemTime::now()));
                                            println!("[{}] Armijo-Goldstein condition passed: {} >= {}", humantime::format_rfc3339(SystemTime::now()), base_error - new_error, step_size * t);
                                        }

                                        if final_loop {
                                            changed_params = changes.iter().filter(|v| **v != 0).count();
                                            total_param_changes += changed_params;
                                            changed_since_step_size_reset = true;
                                            changes_this_feature_set_loop += changed_params;
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
                                        failured_param_changes += 1;
                                        if !quiet_logging || failured_param_changes % if is_fullbatch { 5 } else { 15 } == 0 {
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
                                } else {
                                    change_sets_this_feature += 1;
                                }

                                iterations += 1;
                                let will_save = !quiet_logging || last_disk_save.elapsed() > Duration::from_secs(30);
                                println!(
                                    "[{}] {}error: {new_error:.8}, iterations: {iterations}, step size: {step_size:.1}, biggest change: {biggest_change}, \
                                    changed {changed_params} params, total param changes {total_param_changes}, {} error reduction {:.8}, \
                                    step_size_resets: {step_size_resets}, feature_set: {} ({} left), feature_set_loops: {feature_set_loops}, base_c: {base_c}, \
                                    param changes this feature set loop: {changes_this_feature_set_loop}, error reduction this feature set loop: {:.8}, \
                                    batch: {batch_description}, minibatched_loops_run: {minibatched_loops_run}",
                                    humantime::format_rfc3339(SystemTime::now()),
                                    if will_save { "Saving, " } else { "" },
                                    if is_fullbatch { "total" } else { "batch" },
                                    starting_errors[batch_num - 1] - new_error,
                                    feature_set.name,
                                    working_feature_sets.len(),
                                    starting_error_this_feature_set_loop.unwrap() - new_error,
                                );

                                if will_save {
                                    save_params(&params);
                                    save_gradient(&gradient);
                                    last_disk_save = Instant::now();
                                }

                                if biggest_change == 0 || (!is_fullbatch && change_sets_this_feature >= max_change_sets_per_minibatch) {
                                    break;
                                }
                            }
                        }

                        if !is_fullbatch || changes_this_feature_set_loop == 0 {
                            break;
                        }

                        feature_set_loops += 1;
                        save_params(&params);
                        pretty_print_save_params(&params);
                        last_disk_save = Instant::now();
                    }
                }

            }
        }
    }

    println!("Regression done");
    save_params(&params);
    pretty_print_save_params(&params);
}

pub fn change_param_at_index(i: usize, quiet_positions: bool) -> bool {
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
        // PawnShield uses its own tapering so it does not have an endgame value.
        // There is an extra value included in the params list so that the position of midgame and endgame values stays consistent. Skip that value.
        || i == FeatureIndex::PawnShield as usize + 1
    {
        return false;
    }

    // Not included in tuning because I think this is more of a search issue. Quiet positions will usually not have pawns threaten pieces.
    if !quiet_positions && value_is_between(i, FeatureIndex::PawnsThreatenPieces, FeatureIndex::IsolatedPawns) {
        return false;   
    }
        

    return true;
}

fn search_error_for_params(positions: &mut [TexelPosition], params: &EvalParams, scaling_constant: f64) -> f64 {
    let mut errors = Vec::with_capacity(positions.len());
    let king_attack_unit_values = generate_king_attack_unit_values(&params);

    positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r, &king_attack_unit_values).0;

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

fn qsearch_for_features(positions: &mut [TexelPosition], params: &EvalParams) -> Vec<PositionFeatures> {
    let mut result = Vec::with_capacity(positions.len());
    let king_attack_unit_values = generate_king_attack_unit_values(&params);

    positions
        .par_iter_mut()
        .map_with(MoveRollback::default(), |r, p| {
            let result = p
                .board
                .quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params, r, &king_attack_unit_values);

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

fn get_features_for_quiet_positions(positions: &[TexelPosition]) -> Vec<PositionFeatures> {
    let mut result = Vec::with_capacity(positions.len());
    positions
        .par_iter()
        .map(|p| {
            let features = p
                .board.get_eval_features();

            PositionFeatures {
                features,
                result: p.result,
            }
        })
        .collect_into_vec(&mut result);

    result
}

fn find_error_for_features(
    features: &[PositionFeatures],
    params: &EvalParams,
    scaling_constant: f64,
    king_attack_unit_values: &Box<[i16; 100]>,
) -> f64 {
    let mut errors = Vec::with_capacity(features.len());
    features
        .par_iter()
        .map(|p| {
            let val_sqrt = p.result - sigmoid(p.features.evaluate(params, king_attack_unit_values) as f64, scaling_constant);
            val_sqrt * val_sqrt
        })
        .collect_into_vec(&mut errors);

    let sum = sum_orlp(&errors[..]);
    sum / errors.len() as f64
}

/// params should be unchanged when this method returns
fn eval_gradient(features: &[PositionFeatures], params: &mut EvalParams, scaling_constant: f64, feature_set_range: Range<usize>, quiet: bool, is_quiet_positions: bool) -> Box<EvalGradient> {
    let mut result = Box::new([0f64; EVAL_PARAM_COUNT]);

    for i in feature_set_range {
        // midgame pawns on first row
        if !change_param_at_index(i, is_quiet_positions) {
            continue;
        }

        params[i] += 1;

        let king_attack_unit_values = generate_king_attack_unit_values(params);
        let positive_error = find_error_for_features(features, params, scaling_constant, &king_attack_unit_values);

        params[i] -= 2;

        let king_attack_unit_values = generate_king_attack_unit_values(params);
        let negative_error = find_error_for_features(features, params, scaling_constant, &king_attack_unit_values);

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

pub fn pretty_print_save_params(params: &EvalParams) {
    let mut f = File::create(format!(
        "params/{}-pretty-print.txt",
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis()
    ))
    .unwrap();

    fn write_deinterleaved(f: &mut File, values: &[i16]) {
        let mut second_set = String::new();

        for (i, v) in values.iter().enumerate() {
            if i % 2 == 1 {
                second_set.push_str(format!("{v:4},").as_str());
            } else {
                write!(f, "{v:4},").unwrap();
            }
        }

        // Remove trailing comma
        f.seek(SeekFrom::End(-1)).unwrap();
        writeln!(f, "\n").unwrap();
        write!(f, "{second_set}").unwrap();
        // Remove trailing comma
        f.seek(SeekFrom::End(-1)).unwrap();
        writeln!(f, "\n").unwrap();
    }

    fn write_pair(f: &mut File, params: &EvalParams, index: usize) {
        writeln!(f, "{}, {},\n", params[index], params[index + 1]).unwrap();
    }

    // All of the psqt tables
    for piece in PIECE_PAWN..=PIECE_KING {
        let i = (piece - 1) as usize;
        // midgame and endgame values for the piece
        let value_sets = [&params[i * 64..(i + 1) * 64], &params[(i + 6) * 64..(i + 1 + 6) * 64]];

        for value_set in value_sets {
            for (i, v) in value_set.iter().enumerate() {
                if i % 8 == 7 {
                    writeln!(f, "{v:4},").unwrap();
                } else {
                    write!(f, "{v:4},").unwrap();
                }
            }
            write!(f, "\n").unwrap();
        }
    }

    write_deinterleaved(&mut f, &params[FeatureIndex::PieceValues as usize..FeatureIndex::DoubledPawns as usize]);
    write_pair(&mut f, params, FeatureIndex::DoubledPawns as usize);
    write_pair(&mut f, params, FeatureIndex::PassedPawns as usize);
    write_pair(&mut f, params, FeatureIndex::RookOpenFile as usize);
    write_pair(&mut f, params, FeatureIndex::RookHalfOpenFile as usize);
    write_pair(&mut f, params, FeatureIndex::BishopPair as usize);
    writeln!(f, "{},\n", params[FeatureIndex::PawnShield as usize],).unwrap();

    for i in (FeatureIndex::ConnectedPawns as usize..FeatureIndex::RookMobility as usize).step_by(2) {
        write_pair(&mut f, params, i);
    }

    write_deinterleaved(&mut f, &params[FeatureIndex::RookMobility as usize..FeatureIndex::BishopMobility as usize]);
    write_deinterleaved(&mut f, &params[FeatureIndex::BishopMobility as usize..FeatureIndex::KnightMobility as usize]);
    write_deinterleaved(&mut f, &params[FeatureIndex::KnightMobility as usize..FeatureIndex::QueenMobility as usize]);
    write_deinterleaved(&mut f, &params[FeatureIndex::QueenMobility as usize..FeatureIndex::AttackUnitsCentiMultiplier as usize]);

    writeln!(f, "{},\n", params[FeatureIndex::AttackUnitsCentiMultiplier as usize],).unwrap();
    writeln!(f, "{},\n", params[FeatureIndex::AttackUnitsXOffset as usize],).unwrap();

    // Print any extra values too
    for (i, v) in params[FeatureIndex::AttackUnitsXOffset as usize + 1..EVAL_PARAM_COUNT].iter().enumerate() {
        if i % 8 == 7 {
            writeln!(f, "{v:4},").unwrap();
        } else {
            write!(f, "{v:4},").unwrap();
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
    pub fn evaluate(&self, params: &EvalParams, king_attack_unit_values: &Box<[i16; 100]>) -> i16 {
        let position_score_midgame = FeatureData::sum_psqt_for_a_phase(&self.midgame_psqt_white, &self.midgame_psqt_black, &self.misc_features, params, false);

        let position_score_endgame = FeatureData::sum_psqt_for_a_phase(&self.endgame_psqt_white, &self.endgame_psqt_black, &self.misc_features, params, true);

        let position_score_final = (((position_score_midgame as i32 * self.game_stage as i32)
            + (position_score_endgame as i32 * (MIN_GAME_STAGE_FULLY_MIDGAME as i32 - self.game_stage as i32)))
            / MIN_GAME_STAGE_FULLY_MIDGAME as i32) as i16;

        let untapered_pawn_shield = self.pawn_shield.weight * params[self.pawn_shield.idx as usize];
        let untapered_king_attack_unit = king_attack_unit_values[(Self::sum_attack_units(&self.attack_unit_piece_counts[0], params) / 10).clamp(0, 99) as usize]
            - king_attack_unit_values[(Self::sum_attack_units(&self.attack_unit_piece_counts[1], params) / 10).clamp(0, 99) as usize];
        let king_safety = (self.pawn_shield.taper_amount * (untapered_pawn_shield + untapered_king_attack_unit)) / self.pawn_shield.max_amount;

        position_score_final + king_safety + self.endgame_bonus
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

    fn sum_attack_units(piece_counts: &[u8; 4], params: &EvalParams) -> i16 {
        piece_counts[0] as i16 * params[FeatureIndex::AttackUnitsPieceValues as usize]
            + piece_counts[1] as i16 * params[FeatureIndex::AttackUnitsPieceValues as usize + 1]
            + piece_counts[2] as i16 * params[FeatureIndex::AttackUnitsPieceValues as usize + 2]
            + piece_counts[3] as i16 * params[FeatureIndex::AttackUnitsPieceValues as usize + 3]
    }
}

fn generate_king_attack_unit_values(params: &EvalParams) -> Box<[i16; 100]> {
    let mut result = Box::new([0; 100]);
    
    for (i, v) in result.iter_mut().enumerate() {
        let power = (params[FeatureIndex::AttackUnitsXOffset as usize] as f32 - i as f32) / 10.0;
        *v = ((500.0 * params[FeatureIndex::AttackUnitsCentiMultiplier as usize] as f32 / 100.0) / (1.0 + 4.0_f32.powf(power))).round() as i16
    }

    result
}

fn perturb(params: &mut EvalParams, is_quiet_positions: bool) {
    let mut rand = StdRng::from_entropy();
    for i in 0..params.len() {
        if change_param_at_index(i, is_quiet_positions) {
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

#[derive(Clone)]
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
