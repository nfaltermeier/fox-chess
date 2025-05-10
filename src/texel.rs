use std::{fs::File, io::{BufRead, BufReader}, time::{Instant, SystemTime, UNIX_EPOCH}};
use std::io::Write;

use crate::{board::Board, search::{HistoryTable, Searcher, DEFAULT_HISTORY_TABLE}, transposition_table::TranspositionTable};

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

fn sigmoid(eval: f32, scaling_contant: f32) -> f32 {
    let exp = -eval * scaling_contant / 400.0;
    1.0 / (1.0 + 10.0_f32.powf(exp))
}

pub fn find_scaling_constant(positions: Vec<TexelPosition>) {
    let mut evals = vec![];
    evals.reserve_exact(positions.len());
    let mut history = DEFAULT_HISTORY_TABLE.clone();
    let mut transposition = TranspositionTable::new(2);

    for mut p in positions {
        let mut searcher = Searcher::new(&mut p.board, &mut transposition, &mut history);
        let eval = searcher.quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, &DEFAULT_PARAMS) as f32;
        evals.push((p.result, eval * if p.board.white_to_move { 1.0 } else { -1.0 }));
    }

    let mut scaling_constant = 1.0;
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
            }
        }
    }

    println!("Best scaling constant is {scaling_constant}")
}

fn find_error_from_evals(evals: &Vec<(f32, f32)>, scaling_constant: f32) -> f32 {
    let mut sum = 0.0;

    for e in evals {
        sum += (e.0 - sigmoid(e.1, scaling_constant)).powi(2);
    }

    sum / evals.len() as f32
}

pub fn find_best_params(mut positions: Vec<TexelPosition>) {
    let mut params = DEFAULT_PARAMS.clone();
    let mut history = DEFAULT_HISTORY_TABLE.clone();
    let mut transposition = TranspositionTable::new(2);

    let scaling_constant = 1.0;
    let mut best_error = search_error_for_params(&mut positions, &params, &mut history, &mut transposition, scaling_constant);

    let mut improving = true;
    let mut i = 0;
    while improving {
        improving = false;

        for i in 0..params.len() {
            params[i] += 1;
            let new_error = search_error_for_params(&mut positions, &params, &mut history, &mut transposition, scaling_constant);
            if new_error < best_error {
                best_error = new_error;
                improving = true;
            } else {
                params[i] -= 2;
                let new_error = search_error_for_params(&mut positions, &params, &mut history, &mut transposition, scaling_constant);
                if new_error < best_error {
                    best_error = new_error;
                    improving = true;
                } else {
                    params[i] += 1;
                }
            }
        }

        i += 1;
        if i % 10 == 0 {
            println!("Saving, error: {best_error}, iterations: {i}, time: {}", humantime::format_rfc3339(SystemTime::now()));
            save_params(&params);
        }
    }

    println!("Regression done, error: {best_error}, iterations: {i}");
    save_params(&params);
}

fn search_error_for_params(positions: &mut Vec<TexelPosition>, params: &[i16; 776], history: &mut HistoryTable, tt: &mut TranspositionTable, scaling_constant: f32) -> f32 {
    let mut sum = 0.0;

    for p in &mut *positions {
        let mut searcher = Searcher::new(&mut p.board, tt, history);
        let eval = searcher.quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255, params) as f32;
        let val_sqrt = p.result - sigmoid(eval, scaling_constant);
        sum += val_sqrt * val_sqrt;
    }

    sum / positions.len() as f32
}

fn save_params(params: &[i16; 776]) {
    let mut f = File::create(format!("paths/{}.txt", SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis())).unwrap();
    write!(f, "[").unwrap();

    for (i, v) in params.iter().enumerate() {
        if i % 10 == 9 {
            writeln!(f, "{v},").unwrap();
        } else {
            write!(f, "{v},").unwrap();
        }
    }
}
