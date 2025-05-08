use std::{fs::File, io::{BufRead, BufReader}};

use crate::{board::Board, search::{Searcher, DEFAULT_HISTORY_TABLE}, transposition_table::TranspositionTable};

pub struct TexelPosition {
    pub board: Board,
    pub result: f32,
}

pub fn load_positions(filename: &str) -> Vec<TexelPosition> {
    let mut result = vec![];

    let file = File::open(filename).unwrap();

    for line in BufReader::new(file).lines() {
        let line = line.unwrap();

        if line.len() == 0 {
            continue;
        }

        let c2_index = line.find("c2");
        if c2_index.is_none() {
            panic!("Could not find c2, possibly a malformed line: {}", line);
        }
        let c2_index = c2_index.unwrap();

        let c2 = &line[c2_index..];

        // If forced mate was found or evaluation was skipped because only one move was possible (which may indicate the player is being mated)
        if c2.contains("M") || c2.contains("/1") {
            continue;
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
    let mut transposition = TranspositionTable::new(1);

    for mut p in positions {
        let mut searcher = Searcher::new(&mut p.board, &mut transposition, &mut history);
        let eval = searcher.quiescense_side_to_move_relative(-i16::MAX, i16::MAX, 255) as f32;
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
