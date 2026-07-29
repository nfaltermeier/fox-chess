use crate::board::Board;

// Based on https://github.com/official-stockfish/WDL_model

const MATERIAL_TARGET: f32 = 58.0;
const A_COEFFS: [f32; 4] = [-137.18006228, 364.89416650, -486.05414711, 520.33495197];
const B_COEFFS: [f32; 4] = [0.04907342, -37.48135548, 99.27663650, 48.25721487];

pub fn normalize_score(score: i16, board: &Board) -> i16 {
    let (a, _) = get_parameters(board);
    let normalized_score = 100.0 * score as f32 / a;
    normalized_score.round() as i16
}

fn get_milli_wdl(score: i16, board: &Board) -> (u16, u16, u16) {
    let (w, d, l) = get_wdl(score, board);
    (w.round() as u16, d.round() as u16, l.round() as u16)
}

fn get_wdl(score: i16, board: &Board) -> (f32, f32, f32) {
    let (a, b) = get_parameters(board);
    let w = get_win_rate(score, a, b);
    let l = get_win_rate(-score, a, b);
    let d = 1.0 - w - l;

    (w, d, l)
}

fn get_parameters(board: &Board) -> (f32, f32) {
    let material = board.count_material().clamp(17, 78);
    let material_ratio = material as f32 / MATERIAL_TARGET;
    let a = ((A_COEFFS[0] * material_ratio + A_COEFFS[1]) * material_ratio + A_COEFFS[2]) * material_ratio + A_COEFFS[3];
    let b = ((B_COEFFS[0] * material_ratio + B_COEFFS[1]) * material_ratio + B_COEFFS[2]) * material_ratio + B_COEFFS[3];

    (a, b)
}

fn get_win_rate(score: i16, a: f32, b: f32) -> f32 {
    1.0 / (1.0 + (-(score as f32 - a) / b).exp())
}
