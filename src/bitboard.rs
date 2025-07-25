use array_macro::array;

use crate::{
    board::{Board, PIECE_BISHOP, PIECE_KING, PIECE_KNIGHT, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK},
    magic_bitboard::{COMBINED_BISHOP_RAYS, COMBINED_ROOK_RAYS, lookup_bishop_attack, lookup_rook_attack},
};

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
pub static SQUARES_BETWEEN: [[u64; 64]; 64] = array![x => array![y => squares_in_between(x as u64, y as u64); 64]; 64];

pub struct AttacksTo {
    pub attackers: u64,
    pub possible_rook_like_x_rays: u64,
    pub possible_bishop_like_x_rays: u64,
}

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

/// Code copied from https://www.chessprogramming.org/Square_Attacked_By#Pure_Calculation
/// It is magic I do not understand, but seems to work. Comments are original.
const fn squares_in_between(sq1: u64, sq2: u64) -> u64 {
    let m1 = u64::MAX;
    let a2a7 = 0x0001010101010100;
    let b2g7 = 0x0040201008040200;
    let h1b7 = 0x0002040810204080; /* Thanks Dustin, g2b7 did not work for c1-a3 */

    let btwn = (m1 << sq1) ^ (m1 << sq2);
    let file = (sq2 & 7).wrapping_sub(sq1 & 7);
    let rank = ((sq2 | 7).wrapping_sub(sq1)) >> 3;
    let mut line = ((file & 7).wrapping_sub(1)) & a2a7; /* a2a7 if same file */
    line += 2 * (((rank & 7).wrapping_sub(1)) >> 58); /* b1g1 if same rank */
    line += (((rank.wrapping_sub(file)) & 15).wrapping_sub(1)) & b2g7; /* b2g7 if same diagonal */
    line += (((rank.wrapping_add(file)) & 15).wrapping_sub(1)) & h1b7; /* h1b7 if same antidiag */
    line = line.wrapping_mul(btwn & (!btwn).overflowing_add(1).0); /* mul acts like shift by smaller square */
    line & btwn /* return the bits on that line in-between */
}

pub const fn south_fill(mut b: u64) -> u64 {
    b |= b >> 8;
    b |= b >> 16;
    b |= b >> 32;

    b
}

pub const fn north_fill(mut b: u64) -> u64 {
    b |= b << 8;
    b |= b << 16;
    b |= b << 32;

    b
}

impl Board {
    pub const fn white_passed_pawns(&self) -> u64 {
        let mut front_span =
            south_fill(self.piece_bitboards[1][PIECE_PAWN as usize]) & !self.piece_bitboards[1][PIECE_PAWN as usize];
        front_span |= east_one(front_span) | west_one(front_span);

        let mut blocked_own_pawns = self.piece_bitboards[0][PIECE_PAWN as usize] >> 8;
        blocked_own_pawns = south_fill(blocked_own_pawns);

        self.piece_bitboards[0][PIECE_PAWN as usize] & !front_span & !blocked_own_pawns
    }

    pub const fn black_passed_pawns(&self) -> u64 {
        let mut front_span =
            north_fill(self.piece_bitboards[0][PIECE_PAWN as usize]) & !self.piece_bitboards[0][PIECE_PAWN as usize];
        front_span |= east_one(front_span) | west_one(front_span);

        let mut blocked_own_pawns = self.piece_bitboards[1][PIECE_PAWN as usize] << 8;
        blocked_own_pawns = north_fill(blocked_own_pawns);

        self.piece_bitboards[1][PIECE_PAWN as usize] & !front_span & !blocked_own_pawns
    }

    /// Return value is (open files, half open files)
    pub fn rooks_on_open_files(&self, white: bool) -> (i16, i16) {
        let (side, other_side) = if white { (0, 1) } else { (1, 0) };
        let mut open = 0;
        let mut half_open = 0;

        let mut rooks = self.piece_bitboards[side][PIECE_ROOK as usize];
        while rooks != 0 {
            let rook = BIT_SQUARES[bitscan_forward_and_reset(&mut rooks) as usize];
            let file = north_fill(rook) | south_fill(rook);

            if file & self.piece_bitboards[side][PIECE_PAWN as usize] == 0 {
                if file & self.piece_bitboards[other_side][PIECE_PAWN as usize] == 0 {
                    open += 1;
                } else {
                    half_open += 1;
                }
            }
        }

        (open, half_open)
    }

    /// Assumes the color opposite of self.white_to_move is the one taking the pawn en passant
    pub fn can_en_passant(&self, ep_index: u8, pawn_being_taken_idx: usize) -> bool {
        let color_taking_pawn = if self.white_to_move { 1 } else { 0 };
        let color_pawn_being_taken = if self.white_to_move { 0 } else { 1 };

        let mut potential_pawns = lookup_pawn_attack(ep_index, self.white_to_move)
            & self.piece_bitboards[color_taking_pawn][PIECE_PAWN as usize];
        if potential_pawns == 0 {
            return false;
        }

        let king_idx = self.piece_bitboards[color_taking_pawn][PIECE_KING as usize].trailing_zeros() as u8;
        let half_updated_occ = (self.occupancy & !BIT_SQUARES[pawn_being_taken_idx]) | BIT_SQUARES[ep_index as usize];
        let rook_like = self.piece_bitboards[color_pawn_being_taken][PIECE_QUEEN as usize]
            | self.piece_bitboards[color_pawn_being_taken][PIECE_ROOK as usize];
        let bishop_like = self.piece_bitboards[color_pawn_being_taken][PIECE_QUEEN as usize]
            | self.piece_bitboards[color_pawn_being_taken][PIECE_BISHOP as usize];

        while potential_pawns != 0 {
            let pawn_idx = bitscan_forward_and_reset(&mut potential_pawns);
            let updated_occ = half_updated_occ & !BIT_SQUARES[pawn_idx as usize];

            if lookup_rook_attack(king_idx, updated_occ) & rook_like == 0
                && lookup_bishop_attack(king_idx, updated_occ) & bishop_like == 0
            {
                return true;
            }
        }

        false
    }

    /// Gets psuedolegal attackers to a square for both sides
    pub fn get_attacks_to(&self, square_index: u8, occupancy: u64) -> AttacksTo {
        let mut attackers = 0;
        let queens = self.piece_bitboards[0][PIECE_QUEEN as usize] | self.piece_bitboards[1][PIECE_QUEEN as usize];

        // Moving from target square to potential pawn squares so use opposite colors
        attackers |= lookup_pawn_attack(square_index, false) & self.piece_bitboards[0][PIECE_PAWN as usize];
        attackers |= lookup_pawn_attack(square_index, true) & self.piece_bitboards[1][PIECE_PAWN as usize];

        attackers |= lookup_knight_attack(square_index)
            & (self.piece_bitboards[0][PIECE_KNIGHT as usize] | self.piece_bitboards[1][PIECE_KNIGHT as usize]);

        let bishop_like_squares =
            queens | self.piece_bitboards[0][PIECE_BISHOP as usize] | self.piece_bitboards[1][PIECE_BISHOP as usize];
        attackers |= lookup_bishop_attack(square_index, occupancy) & bishop_like_squares;
        let possible_bishop_like_x_rays =
            COMBINED_BISHOP_RAYS[square_index as usize] & bishop_like_squares & !attackers;

        let rook_like_squares =
            queens | self.piece_bitboards[0][PIECE_ROOK as usize] | self.piece_bitboards[1][PIECE_ROOK as usize];
        attackers |= lookup_rook_attack(square_index, occupancy) & rook_like_squares;
        let possible_rook_like_x_rays = COMBINED_ROOK_RAYS[square_index as usize] & rook_like_squares & !attackers;

        attackers |= lookup_king_attack(square_index)
            & (self.piece_bitboards[0][PIECE_KING as usize] | self.piece_bitboards[1][PIECE_KING as usize]);

        AttacksTo {
            attackers,
            possible_rook_like_x_rays,
            possible_bishop_like_x_rays,
        }
    }

    // Code from https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm
    pub fn get_least_valuable_attacker(&self, attacks: u64, color: usize) -> (u64, usize) {
        let boards = &self.piece_bitboards[color];
        for (piece, board) in boards.iter().enumerate().skip(1) {
            let matches = attacks & board;
            if matches != 0 {
                // Same as matches & -matches
                return (matches & (!matches).wrapping_add(1), piece);
            }
        }

        (0, 0)
    }
}

#[cfg(test)]
mod bitboard_tests {

    use crate::board::Board;

    #[test]
    pub fn basic_passed_pawns() {
        let board = Board::from_fen("7k/3Pp2p/8/8/8/8/1PP1PPp1/K7 w - - 0 1").unwrap();

        assert_eq!(3, board.white_passed_pawns().count_ones());
        assert_eq!(2, board.black_passed_pawns().count_ones());
    }

    #[test]
    pub fn doubled_pawns_are_not_passed_pawns() {
        let board = Board::from_fen("7k/8/6p1/6p1/3P4/3P4/8/K7 w - - 0 1").unwrap();

        assert_eq!(1, board.white_passed_pawns().count_ones());
        assert_eq!(1, board.black_passed_pawns().count_ones());
    }
}
