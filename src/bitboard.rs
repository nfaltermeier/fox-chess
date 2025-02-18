use array_macro::array;

// Little endian rank file mapping
pub const A_FILE: u64 = 0x0101010101010101;
pub const B_FILE: u64 = 0x0202020202020202;
pub const G_FILE: u64 = 0x4040404040404040;
pub const H_FILE: u64 = 0x8080808080808080;
pub const RANK_1: u64 = 0x00000000000000FF;
pub const RANK_2: u64 = 0x000000000000FF00;
pub const RANK_3: u64 = 0x0000000000FF0000;
pub const RANK_6: u64 = 0x0000FF0000000000;
pub const RANK_7: u64 = 0x00FF000000000000;
pub const RANK_8: u64 = 0xFF00000000000000;
pub const LIGHT_SQUARES: u64 = 0x55AA55AA55AA55AA;
pub const DARK_SQUARES: u64 = 0xAA55AA55AA55AA55;

pub static BIT_SQUARES: [u64; 64] = array![i => 1 << i; 64];
static KNIGHT_ATTACKS: [u64; 64] = array![i => generate_knight_attack(BIT_SQUARES[i]); 64];
static KING_ATTACKS: [u64; 64] = array![i => generate_king_attack(BIT_SQUARES[i]); 64];
static PAWN_ATTACKS: [[u64; 64]; 2] = array![x => array![y => generate_pawn_attack(BIT_SQUARES[y], x == 0); 64]; 2];

#[inline]
pub fn lookup_knight_attack(square_bitindex: u8) -> u64 {
    KNIGHT_ATTACKS[square_bitindex as usize]
}

#[inline]
pub fn lookup_king_attack(square_bitindex: u8) -> u64 {
    KING_ATTACKS[square_bitindex as usize]
}

#[inline]
pub fn lookup_pawn_attack(square_bitindex: u8, white: bool) -> u64 {
    PAWN_ATTACKS[if white { 0 } else { 1 }][square_bitindex as usize]
}

const fn generate_knight_attack(knight_position: u64) -> u64 {
    let mut result = 0;

    result |= (knight_position << 17) & !A_FILE;
    result |= (knight_position << 10) & !(A_FILE | B_FILE);
    result |= (knight_position >> 6) & !(A_FILE | B_FILE);
    result |= (knight_position >> 15) & !A_FILE;
    result |= (knight_position << 15) & !H_FILE;
    result |= (knight_position << 6) & !(G_FILE | H_FILE);
    result |= (knight_position >> 10) & !(G_FILE | H_FILE);
    result |= (knight_position >> 17) & !H_FILE;

    result
}

const fn generate_king_attack(mut king_position: u64) -> u64 {
    let mut result = 0;

    result |= east_one(king_position) | west_one(king_position);
    king_position |= result;
    result |= north_one(king_position) | south_one(king_position);

    result
}

const fn generate_pawn_attack(pawn_position: u64, white: bool) -> u64 {
    if white {
        north_east_one(pawn_position) | north_west_one(pawn_position)
    } else {
        south_east_one(pawn_position) | south_west_one(pawn_position)
    }
}

pub const fn north_one(board: u64) -> u64 {
    board << 8
}

const fn east_one(board: u64) -> u64 {
    (board << 1) & !A_FILE
}

pub const fn south_one(board: u64) -> u64 {
    board >> 8
}

const fn west_one(board: u64) -> u64 {
    (board >> 1) & !H_FILE
}

pub const fn north_east_one(board: u64) -> u64 {
    (board << 9) & !A_FILE
}

pub const fn south_east_one(board: u64) -> u64 {
    (board >> 7) & !A_FILE
}

pub const fn south_west_one(board: u64) -> u64 {
    (board >> 9) & !H_FILE
}

pub const fn north_west_one(board: u64) -> u64 {
    (board << 7) & !H_FILE
}

pub fn pretty_print_bitboard(val: u64) -> String {
    let mut result = String::new();

    for i in (0..8).rev() {
        let v = (val & (0xFF << (8 * i))) >> (8 * i);
        result = format!("{result}\n{:08b}", (v as u8).reverse_bits());
    }

    result
}

/// Returns the index of the set bit
pub fn bitscan_forward_and_reset(num: &mut u64) -> u32 {
    let val = num.trailing_zeros();

    *num &= !(1 << val);

    val
}
