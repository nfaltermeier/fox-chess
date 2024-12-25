use std::{fmt::Debug, sync::LazyLock};

use log::error;
use rand::{rngs::StdRng, Fill, SeedableRng};

#[rustfmt::skip]
static DEFAULT_BOARD: [u8; 120] = [
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF,
    0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF,
    0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF,
    0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF,
    0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF,
    0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF,
    0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF,
    0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
];

#[rustfmt::skip]
pub static DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION: [u8; 120] = [
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0xFF,
    0xFF, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0xFF,
    0xFF, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0xFF,
    0xFF, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0xFF,
    0xFF, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0xFF,
    0xFF, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0xFF,
    0xFF, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0xFF,
    0xFF, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
];

#[rustfmt::skip]
// little endian file-rank mapping https://www.chessprogramming.org/Square_Mapping_Considerations
pub static BOARD_SQUARE_INDEX_TRANSLATION_64: [u8; 64] = [
    21, 22, 23, 24, 25, 26, 27, 28,
    31, 32, 33, 34, 35, 36, 37, 38,
    41, 42, 43, 44, 45, 46, 47, 48,
    51, 52, 53, 54, 55, 56, 57, 58,
    61, 62, 63, 64, 65, 66, 67, 68,
    71, 72, 73, 74, 75, 76, 77, 78,
    81, 82, 83, 84, 85, 86, 87, 88,
    91, 92, 93, 94, 95, 96, 97, 98
];

pub const PIECE_MASK: u8 = 0b0000_0111;
pub const PIECE_NONE: u8 = 0;
pub const PIECE_PAWN: u8 = 0x1;
pub const PIECE_KNIGHT: u8 = 0x2;
pub const PIECE_BISHOP: u8 = 0x3;
pub const PIECE_ROOK: u8 = 0x4;
pub const PIECE_QUEEN: u8 = 0x5;
pub const PIECE_KING: u8 = 0x6;
pub const PIECE_INVALID: u8 = 0xFF;

pub const COLOR_FLAG_MASK: u8 = 1 << 3;
pub const COLOR_BLACK: u8 = 1 << 3;

#[derive(Copy, Clone)]
pub enum CastlingValue {
    WhiteKing = 0,
    WhiteQueen = 1,
    BlackKing = 2,
    BlackQueen = 3,
}

pub const CASTLE_WHITE_KING_FLAG: u8 = 1 << CastlingValue::WhiteKing as u8;
pub const CASTLE_WHITE_QUEEN_FLAG: u8 = 1 << CastlingValue::WhiteQueen as u8;
pub const CASTLE_BLACK_KING_FLAG: u8 = 1 << CastlingValue::BlackKing as u8;
pub const CASTLE_BLACK_QUEEN_FLAG: u8 = 1 << CastlingValue::BlackQueen as u8;

pub static HASH_VALUES: LazyLock<[u64; 781]> = LazyLock::new(|| {
    // rand crate doesn't gurantee values are reproducible...
    let mut rng = StdRng::seed_from_u64(0x88d885d4bb51ffc3);
    let mut result = [0; 781];

    if result.try_fill(&mut rng).is_err() {
        error!("Failed to initialize hash values with random data.");
        panic!("Failed to initialize hash values with random data.");
    }

    result
});
pub const HASH_VALUES_BLACK_TO_MOVE_IDX: usize = 12 * 64;
pub const HASH_VALUES_CASTLE_BASE_IDX: usize = HASH_VALUES_CASTLE_WHITE_KING_IDX;
pub const HASH_VALUES_CASTLE_WHITE_KING_IDX: usize = HASH_VALUES_BLACK_TO_MOVE_IDX + 1;
pub const HASH_VALUES_CASTLE_WHITE_QUEEN_IDX: usize = HASH_VALUES_CASTLE_WHITE_KING_IDX + 1;
pub const HASH_VALUES_CASTLE_BLACK_KING_IDX: usize = HASH_VALUES_CASTLE_WHITE_QUEEN_IDX + 1;
pub const HASH_VALUES_CASTLE_BLACK_QUEEN_IDX: usize = HASH_VALUES_CASTLE_BLACK_KING_IDX + 1;
pub const HASH_VALUES_EP_FILE_IDX: usize = HASH_VALUES_CASTLE_BLACK_QUEEN_IDX + 1;

#[derive(Clone, PartialEq, Eq)]
pub struct Board {
    squares: [u8; 120],
    pub white_to_move: bool,
    pub castling_rights: u8,
    pub en_passant_target_square_index: Option<u8>,
    pub halfmove_clock: u8,
    // Is this needed?
    pub fullmove_counter: u16,
    pub hash: u64,
}

impl Board {
    pub fn get_piece(&self, square_index: usize) -> u8 {
        self.squares[square_index]
    }

    pub fn get_piece_64(&self, square_index: usize) -> u8 {
        self.squares[BOARD_SQUARE_INDEX_TRANSLATION_64[square_index] as usize]
    }

    pub fn write_piece(&mut self, piece: u8, square_index: usize) {
        self.squares[BOARD_SQUARE_INDEX_TRANSLATION_64[square_index] as usize] = piece;
    }

    pub fn from_fen(fen: &str) -> Result<Board, String> {
        if !fen.is_ascii() {
            return Err(String::from("Expected FEN to only contain ASCII characters"));
        }

        let fen_pieces: Vec<&str> = fen.split(' ').collect();
        if fen_pieces.len() != 6 {
            return Err(format!(
                "Expected FEN to have 6 space-delimited parts but it had {}",
                fen_pieces.len()
            ));
        }

        let mut board = Board::default();
        let mut board_index: usize = 56;
        let hash_values = &*HASH_VALUES;

        for c in fen_pieces[0].chars() {
            match c {
                '/' => {
                    if board_index < 16 {
                        // Should only happen after the 1st rank is processed...
                        break;
                    }
                    board_index -= 16;
                }
                '1'..='8' => {
                    board_index += char::to_digit(c, 10).unwrap() as usize;
                }
                'P' => {
                    place_piece_init(&mut board, PIECE_PAWN, true, board_index, hash_values);
                    board_index += 1;
                }
                'N' => {
                    place_piece_init(&mut board, PIECE_KNIGHT, true, board_index, hash_values);
                    board_index += 1;
                }
                'B' => {
                    place_piece_init(&mut board, PIECE_BISHOP, true, board_index, hash_values);
                    board_index += 1;
                }
                'R' => {
                    place_piece_init(&mut board, PIECE_ROOK, true, board_index, hash_values);
                    board_index += 1;
                }
                'Q' => {
                    place_piece_init(&mut board, PIECE_QUEEN, true, board_index, hash_values);
                    board_index += 1;
                }
                'K' => {
                    place_piece_init(&mut board, PIECE_KING, true, board_index, hash_values);
                    board_index += 1;
                }
                'p' => {
                    place_piece_init(&mut board, PIECE_PAWN, false, board_index, hash_values);
                    board_index += 1;
                }
                'n' => {
                    place_piece_init(&mut board, PIECE_KNIGHT, false, board_index, hash_values);
                    board_index += 1;
                }
                'b' => {
                    place_piece_init(&mut board, PIECE_BISHOP, false, board_index, hash_values);
                    board_index += 1;
                }
                'r' => {
                    place_piece_init(&mut board, PIECE_ROOK, false, board_index, hash_values);
                    board_index += 1;
                }
                'q' => {
                    place_piece_init(&mut board, PIECE_QUEEN, false, board_index, hash_values);
                    board_index += 1;
                }
                'k' => {
                    place_piece_init(&mut board, PIECE_KING, false, board_index, hash_values);
                    board_index += 1;
                }
                _ => {
                    return Err(format!(
                        "Encountered unexpected character {} while processing piece placement",
                        c
                    ));
                }
            }
        }

        if fen_pieces[1] == "w" {
            board.white_to_move = true;
        } else if fen_pieces[1] == "b" {
            board.white_to_move = false;
            board.hash ^= hash_values[HASH_VALUES_BLACK_TO_MOVE_IDX];
        } else {
            return Err(format!("Encountered unexpected Side to move value '{}'", fen_pieces[1]));
        }

        if fen_pieces[2] != "-" {
            for c in fen_pieces[2].chars() {
                match c {
                    'K' => {
                        board.castling_rights |= CASTLE_WHITE_KING_FLAG;
                        board.hash ^= hash_values[HASH_VALUES_CASTLE_WHITE_KING_IDX];
                    }
                    'Q' => {
                        board.castling_rights |= CASTLE_WHITE_QUEEN_FLAG;
                        board.hash ^= hash_values[HASH_VALUES_CASTLE_WHITE_QUEEN_IDX];
                    }
                    'k' => {
                        board.castling_rights |= CASTLE_BLACK_KING_FLAG;
                        board.hash ^= hash_values[HASH_VALUES_CASTLE_BLACK_KING_IDX];
                    }
                    'q' => {
                        board.castling_rights |= CASTLE_BLACK_QUEEN_FLAG;
                        board.hash ^= hash_values[HASH_VALUES_CASTLE_BLACK_QUEEN_IDX];
                    }
                    _ => {
                        return Err(format!(
                            "Encountered unexpected character {} while processing castling rights",
                            c
                        ));
                    }
                }
            }
        }

        if fen_pieces[3] != "-" {
            let chars: Vec<char> = fen_pieces[3].chars().collect();
            if chars.len() != 2 {
                return Err(format!(
                    "Expected en passant target square value length to be 2 but it was {}. Value: '{}'",
                    chars.len(),
                    fen_pieces[3]
                ));
            }

            let mut ep_square_index;
            match chars[0] {
                'a'..='h' => {
                    ep_square_index = chars[0] as u8 - b'a';
                    board.hash ^= hash_values[HASH_VALUES_EP_FILE_IDX + ep_square_index as usize];
                }
                _ => {
                    return Err(format!(
                        "Encountered unexpected character {} while en passant target square file",
                        chars[0]
                    ));
                }
            }

            match chars[1] {
                '3' | '6' => {
                    ep_square_index += 8 * chars[1].to_digit(10).unwrap() as u8;
                }
                _ => {
                    return Err(format!(
                        "Encountered unexpected character {} while en passant target square rank",
                        chars[1]
                    ));
                }
            }

            board.en_passant_target_square_index = Some(ep_square_index);
        }

        let hmc_result = fen_pieces[4].parse::<u8>();
        match hmc_result {
            Ok(hmc) => {
                board.halfmove_clock = hmc;
            }
            Err(e) => {
                return Err(format!(
                    "Encountered error while parsing halfmove counter value '{}' as u8: {}",
                    fen_pieces[4], e
                ));
            }
        }

        let fmc_result = fen_pieces[5].parse::<u16>();
        match fmc_result {
            Ok(fmc) => {
                board.fullmove_counter = fmc;
            }
            Err(e) => {
                return Err(format!(
                    "Encountered error while parsing fullmove counter value '{}' as u16: {}",
                    fen_pieces[5], e
                ));
            }
        }

        Ok(board)
    }

    fn pretty_print(&self) -> String {
        (0..64)
            .map(|i| piece_to_name(self.get_piece_64(i)).to_string())
            // Could not chunk the map result directly for some reason
            .collect::<Vec<_>>()
            .chunks_exact(8)
            .map(|chunk| chunk.join(", "))
            // Reverse so it prints with a1 in the bottom left like viewing the board as white
            .rev()
            // Join the sets of 8 with newlines
            .reduce(|acc, x| format!("{}\n{}", acc, x))
            .unwrap()
    }
}

impl Default for Board {
    fn default() -> Self {
        Self {
            squares: DEFAULT_BOARD,
            white_to_move: true,
            castling_rights: 0,
            en_passant_target_square_index: None,
            halfmove_clock: 0,
            fullmove_counter: 1,
            hash: 0,
        }
    }
}

impl Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = f
            .debug_struct("Board")
            .field("squares", &"See end value")
            .field("white_to_move", &self.white_to_move)
            .field("castling_rights", &self.castling_rights)
            .field("en_passant_target_square_index", &self.en_passant_target_square_index)
            .field("halfmove_clock", &self.halfmove_clock)
            .field("fullmove_counter", &self.fullmove_counter)
            .field("hash", &format!("{:#018x}", self.hash))
            .finish();
        if result.is_err() {
            panic!("Failed to convert Board to debug struct representation")
        }

        let pretty_squares = self
            .squares
            // Convert everything to hex
            .map(|v| format!("{:#04x}", v))
            // Make sets of 10 squares joined with commas
            .chunks_exact(10)
            .map(|chunk| chunk.join(", "))
            // Reverse so it prints with a1 in the bottom left like viewing the board as white
            .rev()
            // Join the sets of 10 with newlines
            .reduce(|acc, x| format!("{}\n{}", acc, x))
            .unwrap();

        writeln!(f, "\nsquares:\n{}", pretty_squares).unwrap();
        writeln!(f, "pretty version:\n{}", self.pretty_print())
    }
}

pub fn rank_8x8(index: u8) -> u8 {
    ((index & 0x38) >> 3) + 1
}

pub fn file_8x8(index: u8) -> u8 {
    index & 0x07
}

pub fn piece_to_name(piece: u8) -> char {
    let result = match piece & PIECE_MASK {
        PIECE_PAWN => 'P',
        PIECE_KNIGHT => 'N',
        PIECE_BISHOP => 'B',
        PIECE_ROOK => 'R',
        PIECE_QUEEN => 'Q',
        PIECE_KING => 'K',
        PIECE_NONE => ' ',
        _ => panic!("Unexpected piece {piece} passed to piece_to_name"),
    };

    if piece & COLOR_BLACK != 0 {
        result.to_ascii_lowercase()
    } else {
        result
    }
}

pub fn index_10x12_to_pos_str(i: u8) -> String {
    index_8x8_to_pos_str(DEFAULT_BOARD_SQUARE_INDEX_REVERSE_TRANSLATION[i as usize])
}

pub fn index_8x8_to_pos_str(i: u8) -> String {
    let rank = rank_8x8(i);
    let file = file_8x8(i);

    format!("{}{}", (b'a' + file) as char, rank)
}

pub fn get_hash_value(piece_code: u8, white: bool, index: usize, hash_values: &[u64; 781]) -> u64 {
    hash_values[if white { 0 } else { 6 * 64 } + ((piece_code - 1) as usize * 64) + index]
}

// For creating a board from the default board
fn place_piece_init(board: &mut Board, piece_code: u8, white: bool, index: usize, hash_values: &[u64; 781]) {
    board.write_piece(piece_code | if white { 0 } else { COLOR_BLACK }, index);
    board.hash ^= get_hash_value(piece_code, white, index, hash_values);
}
