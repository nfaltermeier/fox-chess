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

static FILES: [u64; 8] = array![i => 0x0101010101010101 << i; 8];
static FILES_FOR_ISOLATED: [u64; 8] = array![i => west_one(FILES[i]) | east_one(FILES[i]); 8];

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

pub const fn generate_pawn_attack(pawn_position: u64, white: bool) -> u64 {
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

    pub fn can_probably_promote(&self) -> bool {
        if self.white_to_move {
            let ready_pawns = self.piece_bitboards[0][PIECE_PAWN as usize] & RANK_7;

            if ready_pawns == 0 {
                return false;
            }

            north_one(ready_pawns) & !self.occupancy != 0
                || (north_east_one(ready_pawns) | north_west_one(ready_pawns)) & self.side_occupancy[1] != 0
        } else {
            let ready_pawns = self.piece_bitboards[1][PIECE_PAWN as usize] & RANK_2;

            if ready_pawns == 0 {
                return false;
            }

            south_one(ready_pawns) & !self.occupancy != 0
                || (south_east_one(ready_pawns) | south_west_one(ready_pawns)) & self.side_occupancy[0] != 0
        }
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

    pub fn score_pawn_shield(&self, color: usize) -> i16 {
        let pawn_advance = if color == 0 { north_one } else { south_one };

        let shield_row = generate_pawn_attack(self.piece_bitboards[color][PIECE_KING as usize], color == 0)
            | pawn_advance(self.piece_bitboards[color][PIECE_KING as usize]);

        let close_pawns = (shield_row & self.piece_bitboards[color][PIECE_PAWN as usize]).count_ones();
        let far_pawns = (pawn_advance(shield_row) & self.piece_bitboards[color][PIECE_PAWN as usize]).count_ones();

        (2 * close_pawns + far_pawns) as i16
    }

    pub fn get_connected_pawns(&self, white: bool) -> u64 {
        let pawns = self.piece_bitboards[if white { 0 } else { 1 }][PIECE_PAWN as usize];

        let mut ne_conn = north_east_one(pawns) & pawns;
        ne_conn |= south_west_one(ne_conn);

        let mut nw_conn = north_west_one(pawns) & pawns;
        nw_conn |= south_east_one(nw_conn);

        ne_conn | nw_conn
    }

    pub fn get_pieces_threatened_by_pawns(&self, white: bool) -> u64 {
        let side = if white { 0 } else { 1 };
        let other_side = if white { 1 } else { 0 };

        generate_pawn_attack(self.piece_bitboards[side][PIECE_PAWN as usize], white)
            & (self.side_occupancy[other_side] & !self.piece_bitboards[other_side][PIECE_PAWN as usize])
    }

    pub fn count_doubled_isolated_pawns(&self) -> (i16, i16) {
        let mut pawn_occupied_files = [0, 0];
        let mut isolated_pawn_files = [0, 0];
        for color in 0..=1 {
            for file_index in 0..8 {
                if self.piece_bitboards[color][PIECE_PAWN as usize] & FILES[file_index] > 0 {
                    pawn_occupied_files[color] += 1;

                    if self.piece_bitboards[color][PIECE_PAWN as usize] & FILES_FOR_ISOLATED[file_index] == 0 {
                        isolated_pawn_files[color] += 1;
                    }
                }
            }
        }

        let net_doubled = (self.piece_bitboards[0][PIECE_PAWN as usize].count_ones() as i16 - pawn_occupied_files[0])
            - (self.piece_bitboards[1][PIECE_PAWN as usize].count_ones() as i16 - pawn_occupied_files[1]);
        let net_isolated = isolated_pawn_files[0] - isolated_pawn_files[1];

        (net_doubled, net_isolated)
    }
}

#[cfg(test)]
mod bitboard_tests {
    use crate::{
        STARTING_FEN,
        board::{Board, PIECE_PAWN},
    };

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

    macro_rules! probably_promote_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;

                    let board = Board::from_fen(input).unwrap();

                    assert_eq!(expected, board.can_probably_promote());
                }
            )*
        }
    }

    probably_promote_test! {
        probably_promote_starting_position: (STARTING_FEN, false),
        white_open: ("k7/4P3/K7/8/8/8/8/8 w - - 0 1", true),
        black_open: ("k7/8/K7/8/8/8/4p3/8 b - - 0 1", true),
        black_sw: ("k7/8/K7/8/8/8/4p3/3R4 b - - 0 1", true),
        black_se: ("k7/8/K7/8/8/8/4p3/5R2 b - - 0 1", true),
        white_sw: ("k2r4/4P3/K7/8/8/8/8/8 w - - 0 1", true),
        white_se: ("k4r2/4P3/K7/8/8/8/8/8 w - - 0 1", true),
        white_self_blocked: ("k3B3/4P3/K7/8/8/8/8/8 w - - 0 1", false),
        white_opponent_blocked: ("4k3/4P3/3K4/8/8/8/8/8 w - - 0 1", false),
        black_self_blocked: ("8/8/K5k1/8/8/8/4p3/4r3 w - - 0 1", false),
        black_opponent_blocked: ("8/8/8/8/8/3k4/4p3/4K3 b - - 0 1", false),
    }

    macro_rules! pawn_shield_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (fen, expected_white_score, expected_black_score) = $value;

                    let board = Board::from_fen(fen).unwrap();
                    let white_score = board.score_pawn_shield(0);
                    let black_score = board.score_pawn_shield(1);

                    assert_eq!(expected_white_score, white_score);
                    assert_eq!(expected_black_score, black_score);
                }
            )*
        }
    }

    pawn_shield_test! {
        single_close_pawn: ("1k6/1p6/8/8/8/8/6P1/6K1 w - - 0 1", 2, 2),
        single_far_pawn: ("1k6/8/1p6/8/8/6P1/8/6K1 w - - 0 1", 1, 1),
        full_close_shields: ("1k6/ppp5/8/8/8/8/5PPP/6K1 w - - 0 1", 6, 6),
        mixed_shields: ("1k6/1pp5/p7/8/8/6PP/5P2/6K1 w - - 0 1", 4, 5),
        completely_full_shields: ("1k6/ppp5/ppp5/8/8/5PPP/5PPP/6K1 w - - 0 1", 9, 9),
        shield_not_position_dependent: ("8/4k3/3ppp2/5P2/4P3/4K3/8/8 w - - 0 1", 3, 6),
    }

    macro_rules! connected_pawns_count_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (fen, expected_white_count, expected_black_count) = $value;

                    let board = Board::from_fen(fen).unwrap();
                    let white = board.get_connected_pawns(true);
                    let black = board.get_connected_pawns(false);

                    assert_eq!(expected_white_count, white.count_ones());
                    assert_eq!(expected_black_count, black.count_ones());
                }
            )*
        }
    }

    connected_pawns_count_test! {
        v: ("rnbqkbnr/pppp4/4p3/5p1p/2P3pP/1P1P4/P3PPP1/RNBQKBNR w KQkq - 0 6", 5, 5),
        pairs: ("8/8/p2ppp2/1p1p2pp/1P1P2PP/P2PPP2/8/1K1k4 w - - 0 1", 6, 6),
        long_chain: ("8/5Pp1/4Pp2/3Pp3/2Pp4/1Pp5/Pp6/1K1k4 w - - 0 1", 6, 6),
        x: ("8/8/5p1p/1P1P2p1/2P2p1p/1P1P4/8/1K1k4 w - - 0 1", 5, 5),
        unpaired: ("8/2P1ppPP/1p6/1P1p3p/p2P4/P1p3p1/4PP2/1K1k4 w - - 0 1", 0, 0),
        checkered: ("8/8/1P1Pp1p1/p1p2P1P/1P4p1/p1p2P1P/1P1Pp1p1/1K1k4 w - - 0 1", 0, 0),
    }

    /// All pawns must be connected
    macro_rules! connected_pawns_shape_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let fen = $value;

                    let board = Board::from_fen(fen).unwrap();
                    let white = board.get_connected_pawns(true);
                    let black = board.get_connected_pawns(false);

                    assert_eq!(board.piece_bitboards[0][PIECE_PAWN as usize], white);
                    assert_eq!(board.piece_bitboards[1][PIECE_PAWN as usize], black);
                }
            )*
        }
    }

    connected_pawns_shape_test! {
        shape_x: "8/8/5p1p/1P1P2p1/2P2p1p/1P1P4/8/1K1k4 w - - 0 1",
        shape_pairs: "8/8/p3pp2/1p1p2p1/1P1P2P1/P3PP2/8/1K1k4 w - - 0 1",
        shape_long_chain: "8/5Pp1/4Pp2/3Pp3/2Pp4/1Pp5/Pp6/1K1k4 w - - 0 1",
        shape_v: "4k3/3p4/4p3/5p1p/2P3p1/1P1P4/P3P3/4K3 w - - 0 6",
    }

    /// All pawns must be connected
    macro_rules! pieces_threatened_by_pawns_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (fen, w_value, b_value) = $value;

                    let board = Board::from_fen(fen).unwrap();
                    let white = board.get_pieces_threatened_by_pawns(true);
                    let black = board.get_pieces_threatened_by_pawns(false);

                    assert_eq!(white.count_ones(), w_value);
                    assert_eq!(black.count_ones(), b_value);
                }
            )*
        }
    }

    pieces_threatened_by_pawns_test! {
        pawns_threaten_pawns: ("1k6/8/2p1p3/1p1p4/2P1P3/3P4/8/5K2 w - - 0 1", 0, 0),
        each_type_threaten_twice: ("1k6/8/r1b1n1q1/1P1P1P1P/1p1p1p1p/Q1R1B1N1/8/5K2 w - - 0 1", 4, 4),
        king_threatened: ("3k4/4P3/8/8/8/8/2p5/3K4 w - - 0 1", 1, 1),
    }

    macro_rules! doubled_pawns_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;

                    let board = Board::from_fen(input).unwrap();
                    let doubled_pawns = board.count_doubled_isolated_pawns().0;

                    assert_eq!(expected, doubled_pawns);
                }
            )*
        }
    }

    doubled_pawns_test! {
        doubled_pawns_starting_position: (STARTING_FEN, 0),
        white_two_doubled: ("rnbqkbnr/pppppppp/8/8/8/1P4P1/PP1PP1PP/RNBQKBNR w KQkq - 0 1", 2),
        black_two_doubled: ("rnbqkbnr/1ppp1ppp/1p5p/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", -2),
        white_tripled: ("rnbqkbnr/pppppppp/8/8/3P4/3P4/PP1P1PPP/RNBQKBNR w KQkq - 0 1", 2),
        black_tripled: ("rnbqkbnr/1ppp1ppp/1p6/1p6/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", -2),
        white_only_single_pawn: ("1k6/8/8/8/8/8/4P3/4K3 w - - 0 1", 0),
        white_only_doubled_pawn: ("1k6/8/8/8/8/4P3/4P3/4K3 w - - 0 1", 1),
        black_only_single_pawn: ("1k6/1p6/8/8/8/8/8/4K3 w - - 0 1", 0),
        black_only_doubled_pawn: ("1k6/1p6/1p6/8/8/8/8/4K3 w - - 0 1", -1),
        unbalanced: ("1k6/1p2pp2/1p6/8/8/4P1P1/4P1P1/4K3 w - - 0 1", 1),
        unbalanced_opposite_colors: ("1K6/1P2PP2/1P6/8/8/4p1p1/4p1p1/4k3 w - - 0 1", -1),
    }
}
