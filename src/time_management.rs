use std::{collections::VecDeque, time::{Duration, Instant}};

use build_info::chrono::TimeDelta;
use log::error;

pub struct TimeResult {
    pub soft_cutoff: Duration,
    pub hard_cutoff: Instant,
    stricter_time_cutoff: bool,
    flexible_cutoff: bool,
}

pub fn get_cutoff_times(
    time_left: &Option<TimeDelta>,
    increment: &Option<TimeDelta>,
    start_time: &Instant,
    fullmove_counter: u16,
) -> TimeResult {
    if time_left.is_none() {
        error!("No time left value provided when searching");
        panic!("No time left value provided when searching");
    }

    let divisor = if fullmove_counter < 15 {
        25
    } else if fullmove_counter < 25 {
        20
    } else {
        30
    };

    let time_left = time_left.as_ref().unwrap().to_std().unwrap();
    let mut target_dur = time_left.checked_div(divisor).unwrap();

    let stricter_time_cutoff;
    let flexible_cutoff;
    if let Some(inc) = increment {
        let inc = inc.to_std().unwrap();

        if time_left > inc.saturating_mul(2) {
            target_dur = target_dur.saturating_add(inc.mul_f32(0.7));

            stricter_time_cutoff = time_left < Duration::from_secs(1);
            flexible_cutoff = time_left > inc.saturating_mul(4);
        } else {
            stricter_time_cutoff = true;
            flexible_cutoff = false;
        }
    } else {
        stricter_time_cutoff = time_left < Duration::from_secs(5);
        flexible_cutoff = false;
    }

    TimeResult {
        soft_cutoff: target_dur.mul_f32(if stricter_time_cutoff { 0.3 } else { 0.5 }),
        hard_cutoff: start_time
            .checked_add(target_dur.mul_f32(if stricter_time_cutoff { 1.1 } else { 2.0 }))
            .unwrap(),
        stricter_time_cutoff,
        flexible_cutoff,
    }
}

pub fn modify_cutoff_time(base_time_data: &TimeResult, pv_nodes_fractions: &VecDeque<f32>) -> Duration {
    if !base_time_data.flexible_cutoff || base_time_data.stricter_time_cutoff {
        return base_time_data.soft_cutoff;
    }

    let avg_pv_nodes_fraction = pv_nodes_fractions.iter().sum::<f32>() / pv_nodes_fractions.len() as f32;

    // f(0) ~= 1.2, f(.54) ~= 1, f(.8) ~= .76, f(1) ~= .5
    let time_modifier = 1.19573 + (0.02241993 * avg_pv_nodes_fraction) - (0.7081851 * avg_pv_nodes_fraction.powi(2));

    base_time_data.soft_cutoff.mul_f32(time_modifier)
}
