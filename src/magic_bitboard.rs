use array_macro::array;

use crate::bitboard::{A_FILE, BIT_SQUARES, H_FILE, RANK_1, RANK_8};

static mut ATTACKS: Vec<u64> = Vec::new();
static mut ROOK_ATTACK_LOOKUP: [MagicEntry; 64] = array![_ => MagicEntry::const_default(); 64];
static mut BISHOP_ATTACK_LOOKUP: [MagicEntry; 64] = array![_ => MagicEntry::const_default(); 64];

struct MagicEntry {
    attacks_offset: u16,
    mask: u64,
    magic: u64,
    shift: u8,
}

impl MagicEntry {
    const fn const_default() -> MagicEntry {
        MagicEntry { attacks_offset: 0, mask: 0, magic: 0, shift: 0 }
    }
}

/// Should be called once on application startup
pub fn initialize_magic_bitboards() {
    let attacks = &raw mut ATTACKS;
    unsafe {
        for i in 0..64 {
            let entry = &raw mut ROOK_ATTACK_LOOKUP[i];
            let mask = generate_rook_relevant_occupancy(i as u8);
            let shift = (*entry).shift;
            let magic = (*entry).magic;
            let attacks_offset = (*attacks).len();

            (*entry).mask = mask;
            (*entry).attacks_offset = attacks_offset as u16;
            (*attacks).reserve(1 << shift);
            for _ in 0..(1 << shift) {
                (*attacks).push(0);
            }

            for occupancy_value in 0..(1 << mask.count_ones()) {
                let occupancy = map_value_to_mask(occupancy_value, mask);

                let mut attack_index = occupancy & mask;
                attack_index *= magic;
                attack_index >>= shift;

                // TODO: generate attack from occupancy mask
                let attack = generate_rook_attack(i as u8);
                let attack_list_entry = &mut (*attacks)[attacks_offset + attack_index as usize];
                debug_assert!(*attack_list_entry == 0 || *attack_list_entry == attack);
                *attack_list_entry = attack;
            }
        }
    }
}

const fn generate_rook_attack(square_index: u8) -> u64 {
    // Seems like it would be better to use rank and file mask functions instead to mask off a filled in bit board?
    let north = 0x0101010101010100 << square_index;
    let east = 2 * ((1 << (square_index | 7)) - (1 << square_index));
    let south = 0x0080808080808080 >> (square_index ^ 63);
    let west = (1 << square_index) - (1 << (square_index & 56));

    north | east | south | west
}

const fn generate_rook_relevant_occupancy(square_index: u8) -> u64 {
    let north = 0x0001010101010100 << square_index;
    let east = (2 * ((1 << (square_index | 7)) - (1 << square_index))) & !H_FILE;
    let south = 0x0080808080808000 >> (square_index ^ 63);
    let west = ((1 << square_index) - (1 << (square_index & 56))) & !A_FILE;

    north | east | south | west
}

// Code from https://www.chessprogramming.org/On_an_empty_Board#By_Calculation_3.
// I don't really get it so variable names may not be helpful.
const fn generate_bishop_attack(square_index: u8) -> u64 {
    let main_diagonal = 0x8040201008040201;
    let main_anti_diagonal = 0x0102040810204080;
    let i_sq = square_index as i32;

    let diagonal_value = (i_sq & 7) - (i_sq >> 3);
    let anti_diagonal_value = 7 - (diagonal_value);

    let diagonal_bits = if diagonal_value >= 0 {
        main_diagonal >> (diagonal_value * 8)
    } else {
        main_diagonal << (-diagonal_value * 8)
    };
    let anti_diagonal_bits = if anti_diagonal_value >= 0 {
        main_anti_diagonal >> (anti_diagonal_value * 8)
    } else {
        main_anti_diagonal << (-anti_diagonal_value * 8)
    };

    (diagonal_bits | anti_diagonal_bits) & !BIT_SQUARES[square_index as usize]
}

const fn generate_bishop_relevant_occupancy(square_index: u8) -> u64 {
    generate_bishop_attack(square_index) & !A_FILE & !H_FILE & !RANK_1 & !RANK_8
}

fn map_value_to_mask(value: u64, mask: u64) -> u64 {
    let bits = mask.count_ones();
    let mut result = 0;
    let mut working_mask = mask;

    for i in 0..bits {
        let mask_index = working_mask.leading_zeros();
        working_mask &= 1 << mask_index;

        if value & (1 << i) != 0 {
            result |= 1 << mask_index;
        }
    }

    result
}
