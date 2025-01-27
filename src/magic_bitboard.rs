use array_macro::array;
use log::debug;

use crate::bitboard::{pretty_print_bitboard, A_FILE, BIT_SQUARES, H_FILE, RANK_1, RANK_8};

const NORTH: usize = 0;
const EAST: usize = 1;
const SOUTH: usize = 2;
const WEST: usize = 3;

const NORTH_EAST: usize = 0;
const SOUTH_EAST: usize = 1;
const SOUTH_WEST: usize = 2;
const NORTH_WEST: usize = 3;

static mut ATTACKS: Vec<u64> = Vec::new();

static ROOK_RAYS: [[u64; 4]; 65] = array![i => if i < 64 { generate_rook_rays(i as u8) } else { [0; 4] }; 65];
static BISHOP_RAYS: [[u64; 4]; 65] = array![i => if i < 64 { generate_bishop_rays(i as u8) } else { [0; 4] }; 65];

struct MagicEntry {
    attacks_offset: u16,
    mask: u64,
    magic: u64,
    shift: u8,
}

impl MagicEntry {
    const fn new(magic: u64, shift: u8) -> MagicEntry {
        MagicEntry {
            attacks_offset: 0,
            mask: 0,
            magic,
            shift,
        }
    }
}

pub fn lookup_rook_attack(square_index: u8, occupancy: u64) -> u64 {
    unsafe {
        let entry = &ROOK_ATTACK_LOOKUP[square_index as usize];

        let mut attack_index = occupancy & entry.mask;
        attack_index = attack_index.wrapping_mul(entry.magic);
        attack_index >>= 64 - entry.shift;

        ATTACKS[entry.attacks_offset as usize + attack_index as usize]
    }
}

/// Should be called once on application startup
pub fn initialize_magic_bitboards() {
    let attacks = &raw mut ATTACKS;

    unsafe {
        (*attacks).reserve_exact(107648);

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
                attack_index = attack_index.wrapping_mul(magic);
                attack_index >>= 64 - shift;

                let attack = generate_occluded_rook_attack(i as u8, occupancy);
                let attack_list_entry = &mut (*attacks)[attacks_offset + attack_index as usize];
                debug_assert!(*attack_list_entry == 0 || *attack_list_entry == attack);
                // if *attack_list_entry != 0 && *attack_list_entry != attack {
                //     debug!("root_position_num: {i}");
                //     debug!("rook_position: {}", pretty_print_bitboard(BIT_SQUARES[i]));
                //     debug!("occupancy: {}", pretty_print_bitboard(occupancy));
                //     debug!("attack: {}", pretty_print_bitboard(attack));
                //     debug!("*attack_list_entry: {}", pretty_print_bitboard(*attack_list_entry));
                //     panic!("attack collision")
                // }
                *attack_list_entry = attack;
            }
        }

        for i in 0..64 {
            let entry = &raw mut BISHOP_ATTACK_LOOKUP[i];
            let mask = generate_bishop_relevant_occupancy(i as u8);
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
                attack_index = attack_index.wrapping_mul(magic);
                attack_index >>= 64 - shift;

                let attack = generate_occluded_bishop_attack(i as u8, occupancy);
                let attack_list_entry = &mut (*attacks)[attacks_offset + attack_index as usize];
                debug_assert!(*attack_list_entry == 0 || *attack_list_entry == attack);
                *attack_list_entry = attack;
            }
        }

        debug!("attacks size: {}", (*attacks).len());
    }
}

const fn generate_rook_attack(square_index: u8) -> u64 {
    let rays = &ROOK_RAYS[square_index as usize];

    rays[NORTH] | rays[EAST] | rays[SOUTH] | rays[WEST]
}

const fn generate_rook_relevant_occupancy(square_index: u8) -> u64 {
    let rays = &ROOK_RAYS[square_index as usize];

    (rays[NORTH] & !RANK_1) | (rays[EAST] & !H_FILE) | (rays[SOUTH] & !RANK_8) | (rays[WEST] & !A_FILE)
}

fn generate_occluded_rook_attack(square_index: u8, occupancy: u64) -> u64 {
    get_occluded_positive_ray(square_index, occupancy, NORTH, &ROOK_RAYS)
        | get_occluded_positive_ray(square_index, occupancy, EAST, &ROOK_RAYS)
        | get_occluded_negative_ray(square_index, occupancy, SOUTH, &ROOK_RAYS)
        | get_occluded_negative_ray(square_index, occupancy, WEST, &ROOK_RAYS)
}

fn generate_occluded_bishop_attack(square_index: u8, occupancy: u64) -> u64 {
    get_occluded_positive_ray(square_index, occupancy, NORTH_EAST, &BISHOP_RAYS)
        | get_occluded_positive_ray(square_index, occupancy, NORTH_WEST, &BISHOP_RAYS)
        | get_occluded_negative_ray(square_index, occupancy, SOUTH_EAST, &BISHOP_RAYS)
        | get_occluded_negative_ray(square_index, occupancy, SOUTH_WEST, &BISHOP_RAYS)
}

const fn generate_bishop_attack(square_index: u8) -> u64 {
    let rays = &BISHOP_RAYS[square_index as usize];

    rays[NORTH_EAST] | rays[SOUTH_EAST] | rays[SOUTH_WEST] | rays[NORTH_WEST]
}

const fn generate_bishop_relevant_occupancy(square_index: u8) -> u64 {
    generate_bishop_attack(square_index) & !A_FILE & !H_FILE & !RANK_1 & !RANK_8
}

fn map_value_to_mask(value: u64, mask: u64) -> u64 {
    let bits = mask.count_ones();
    let mut result = 0;
    let mut working_mask = mask;

    for i in 0..bits {
        let mask_index = working_mask.trailing_zeros();
        working_mask &= 1 << mask_index;

        if value & (1 << i) != 0 {
            result |= 1 << mask_index;
        }
    }

    result
}

const fn generate_rook_rays(square_index: u8) -> [u64; 4] {
    let mut result = [0; 4];

    result[NORTH] = 0x0101010101010100 << square_index;
    result[EAST] = 2 * ((1 << (square_index | 7)) - (1 << square_index));
    result[SOUTH] = 0x0080808080808080 >> (square_index ^ 63);
    result[WEST] = (1 << square_index) - (1 << (square_index & 56));

    result
}

// Code from https://www.chessprogramming.org/On_an_empty_Board#By_Calculation_3.
// I don't really get it so variable names may not be helpful.
const fn generate_bishop_rays(square_index: u8) -> [u64; 4] {
    let mut result = [0; 4];

    let main_diagonal: u64 = 0x8040201008040201;
    let main_anti_diagonal: u64 = 0x0102040810204080;
    let i_sq = square_index as i32;

    let diagonal_value = (i_sq & 7) - (i_sq >> 3);
    let diagonal_bits = if diagonal_value >= 0 {
        main_diagonal >> (diagonal_value * 8)
    } else {
        main_diagonal << (-diagonal_value * 8)
    } & !BIT_SQUARES[square_index as usize];

    let anti_diagonal_value = 7 - (i_sq & 7) - (i_sq >> 3);
    let anti_diagonal_bits = if anti_diagonal_value >= 0 {
        // main_anti_diagonal.overflowing_shr((anti_diagonal_value * 8) as u32).0
        main_anti_diagonal >> ((anti_diagonal_value * 8) as u32)
    } else {
        main_anti_diagonal << (-anti_diagonal_value * 8)
    } & !BIT_SQUARES[square_index as usize];

    result[NORTH_EAST] = diagonal_bits & (0_u64.wrapping_sub(BIT_SQUARES[square_index as usize].wrapping_mul(2)));
    result[NORTH_WEST] = anti_diagonal_bits & (0_u64.wrapping_sub(BIT_SQUARES[square_index as usize].wrapping_mul(2)));
    result[SOUTH_WEST] = diagonal_bits & (BIT_SQUARES[square_index as usize] - 1);
    result[SOUTH_EAST] = anti_diagonal_bits & (BIT_SQUARES[square_index as usize] - 1);

    result
}

const fn get_occluded_positive_ray(square_index: u8, occupancy: u64, direction: usize, rays: &[[u64; 4]; 65]) -> u64 {
    let ray = rays[square_index as usize][direction];
    let blockers = ray & occupancy;

    if blockers != 0 {
        let blocker_square = blockers.trailing_zeros();
        ray ^ rays[blocker_square as usize][direction]
    } else {
        ray
    }
}

const fn get_occluded_negative_ray(square_index: u8, occupancy: u64, direction: usize, rays: &[[u64; 4]; 65]) -> u64 {
    let ray = rays[square_index as usize][direction];
    let blockers = ray & occupancy;

    if blockers != 0 {
        let blocker_square = 64 - blockers.leading_zeros();
        let blocked_ray = rays[blocker_square as usize][direction];
        ray ^ blocked_ray
    } else {
        ray
    }
}

static mut ROOK_ATTACK_LOOKUP: [MagicEntry; 64] = [
    MagicEntry::new(0x80004000976080, 12),
    MagicEntry::new(0x1040400010002000, 11),
    MagicEntry::new(0x4880200210000980, 11),
    MagicEntry::new(0x5280080010000482, 11),
    MagicEntry::new(0x200040200081020, 11),
    MagicEntry::new(0x2100080100020400, 11),
    MagicEntry::new(0x4280008001000200, 11),
    MagicEntry::new(0x1000a4425820300, 12),
    MagicEntry::new(0x29002100800040, 11),
    MagicEntry::new(0x4503400040201004, 10),
    MagicEntry::new(0x209002001004018, 10),
    MagicEntry::new(0x1131000a10002100, 10),
    MagicEntry::new(0x9000800120500, 10),
    MagicEntry::new(0x10e001804820010, 10),
    MagicEntry::new(0x29000402000100, 10),
    MagicEntry::new(0x2002000d01c40292, 11),
    MagicEntry::new(0x80084000200c40, 11),
    MagicEntry::new(0x10004040002002, 10),
    MagicEntry::new(0x201030020004014, 10),
    MagicEntry::new(0x80012000a420020, 10),
    MagicEntry::new(0x129010008001204, 10),
    MagicEntry::new(0x6109010008040002, 10),
    MagicEntry::new(0x950010100020004, 10),
    MagicEntry::new(0x803a0000c50284, 11),
    MagicEntry::new(0x80004100210080, 11),
    MagicEntry::new(0x200240100140, 10),
    MagicEntry::new(0x20004040100800, 10),
    MagicEntry::new(0x4018090300201000, 10),
    MagicEntry::new(0x4802010a00102004, 10),
    MagicEntry::new(0x2001000900040002, 10),
    MagicEntry::new(0x4a02104001002a8, 10),
    MagicEntry::new(0x2188108200204401, 11),
    MagicEntry::new(0x40400020800080, 11),
    MagicEntry::new(0x880402000401004, 10),
    MagicEntry::new(0x10040800202000, 10),
    MagicEntry::new(0x604410a02001020, 10),
    MagicEntry::new(0x200200206a001410, 10),
    MagicEntry::new(0x86000400810080, 10),
    MagicEntry::new(0x428200040600080b, 10),
    MagicEntry::new(0x2001000041000082, 11),
    MagicEntry::new(0x80002000484000, 11),
    MagicEntry::new(0x210002002c24000, 10),
    MagicEntry::new(0x401a200100410014, 10),
    MagicEntry::new(0x5021000a30009, 10),
    MagicEntry::new(0x218000509010010, 10),
    MagicEntry::new(0x4000400410080120, 10),
    MagicEntry::new(0x20801040010, 10),
    MagicEntry::new(0x29040040820011, 11),
    MagicEntry::new(0x4080400024800280, 11),
    MagicEntry::new(0x500200040100440, 10),
    MagicEntry::new(0x2880142001004100, 10),
    MagicEntry::new(0x412020400a001200, 10),
    MagicEntry::new(0x18c028004080080, 10),
    MagicEntry::new(0x884001020080401, 10),
    MagicEntry::new(0x210810420400, 10),
    MagicEntry::new(0x801048745040200, 11),
    MagicEntry::new(0x4401002040120082, 12),
    MagicEntry::new(0x408200210012, 11),
    MagicEntry::new(0x110008200441, 11),
    MagicEntry::new(0x2010002004100901, 11),
    MagicEntry::new(0x801000800040211, 11),
    MagicEntry::new(0x480d000400820801, 11),
    MagicEntry::new(0x820104201280084, 11),
    MagicEntry::new(0x1001040311802142, 12),
];
static mut BISHOP_ATTACK_LOOKUP: [MagicEntry; 64] = [
    MagicEntry::new(0x1024b002420160, 6),
    MagicEntry::new(0x1008080140420021, 5),
    MagicEntry::new(0x2012080041080024, 5),
    MagicEntry::new(0xc282601408c0802, 5),
    MagicEntry::new(0x2004042000000002, 5),
    MagicEntry::new(0x12021004022080, 5),
    MagicEntry::new(0x880414820100000, 5),
    MagicEntry::new(0x4501002211044000, 6),
    MagicEntry::new(0x20402222121600, 5),
    MagicEntry::new(0x1081088a28022020, 5),
    MagicEntry::new(0x1004c2810851064, 5),
    MagicEntry::new(0x2040080841004918, 5),
    MagicEntry::new(0x1448020210201017, 5),
    MagicEntry::new(0x4808110108400025, 5),
    MagicEntry::new(0x10504404054004, 5),
    MagicEntry::new(0x800010422092400, 5),
    MagicEntry::new(0x40000870450250, 5),
    MagicEntry::new(0x402040408080518, 5),
    MagicEntry::new(0x1000980a404108, 7),
    MagicEntry::new(0x1020804110080, 7),
    MagicEntry::new(0x8200c02082005, 7),
    MagicEntry::new(0x40802009a0800, 7),
    MagicEntry::new(0x1000201012100, 5),
    MagicEntry::new(0x111080200820180, 5),
    MagicEntry::new(0x904122104101024, 5),
    MagicEntry::new(0x4008200405244084, 5),
    MagicEntry::new(0x44040002182400, 7),
    MagicEntry::new(0x4804080004021002, 9),
    MagicEntry::new(0x6401004024004040, 9),
    MagicEntry::new(0x404010001300a20, 7),
    MagicEntry::new(0x428020200a20100, 5),
    MagicEntry::new(0x300460100420200, 5),
    MagicEntry::new(0x404200c062000, 5),
    MagicEntry::new(0x22101400510141, 5),
    MagicEntry::new(0x104044400180031, 7),
    MagicEntry::new(0x2040040400280211, 9),
    MagicEntry::new(0x8020400401010, 9),
    MagicEntry::new(0x20100110401a0040, 7),
    MagicEntry::new(0x100101005a2080, 5),
    MagicEntry::new(0x1a008300042411, 5),
    MagicEntry::new(0x120a025004504000, 5),
    MagicEntry::new(0x4001084242101000, 5),
    MagicEntry::new(0xa020202010a4200, 7),
    MagicEntry::new(0x4000002018000100, 7),
    MagicEntry::new(0x80104000044, 7),
    MagicEntry::new(0x1004009806004043, 7),
    MagicEntry::new(0x100401080a000112, 5),
    MagicEntry::new(0x1041012101000608, 5),
    MagicEntry::new(0x40400c250100140, 5),
    MagicEntry::new(0x80a10460a100002, 5),
    MagicEntry::new(0x2210030401240002, 5),
    MagicEntry::new(0x6040aa108481b20, 5),
    MagicEntry::new(0x4009004050410002, 5),
    MagicEntry::new(0x8106003420200e0, 5),
    MagicEntry::new(0x1410500a08206000, 5),
    MagicEntry::new(0x92548802004000, 5),
    MagicEntry::new(0x1040041241028, 6),
    MagicEntry::new(0x120042025011, 5),
    MagicEntry::new(0x8060104054400, 5),
    MagicEntry::new(0x20004404020a0a01, 5),
    MagicEntry::new(0x40008010020214, 5),
    MagicEntry::new(0x4000050209802c1, 5),
    MagicEntry::new(0x208244210400, 5),
    MagicEntry::new(0x10140848044010, 6),
];
