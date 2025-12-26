use std::{fmt::Debug, sync::LazyLock};

use log::error;
use rand::{Fill, SeedableRng, rngs::StdRng};

use crate::{
    bitboard::{BIT_SQUARES, DARK_SQUARES, pretty_print_bitboard}, eval_values::PIECE_SQUARE_TABLES, evaluate::GAME_STAGE_VALUES, repetition_tracker::RepetitionTracker
};

pub const PIECE_MASK: u8 = 0b0000_0111;
pub const PIECE_NONE: u8 = 0;
pub const PIECE_PAWN: u8 = 0x1;
pub const PIECE_KNIGHT: u8 = 0x2;
pub const PIECE_BISHOP: u8 = 0x3;
pub const PIECE_ROOK: u8 = 0x4;
pub const PIECE_QUEEN: u8 = 0x5;
pub const PIECE_KING: u8 = 0x6;

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
    let mut rng = StdRng::seed_from_u64(0x88d885d4bb51ffc2);
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

pub const BISHOP_COLORS_LIGHT: u8 = 1;
pub const BISHOP_COLORS_DARK: u8 = 2;

#[derive(Clone, PartialEq, Eq)]
pub struct Board {
    squares: [u8; 64],
    pub white_to_move: bool,
    pub castling_rights: u8,
    pub en_passant_target_square_index: Option<u8>,
    pub halfmove_clock: u8,
    pub fullmove_counter: u16,
    pub hash: u64,
    pub game_stage: i16,
    pub repetitions: RepetitionTracker,
    /// White then black, pieces are stored by their piece index so 0 is nothing, 1 is pawn, etc.
    pub piece_counts: [[u8; 7]; 2],
    /// White then black, pieces are stored by their piece index so 0 is nothing, 1 is pawn, etc.
    pub piece_bitboards: [[u64; 7]; 2],
    pub side_occupancy: [u64; 2],
    pub occupancy: u64,
    pub piecesquare_midgame: i16,
    pub piecesquare_endgame: i16,
    pub bishop_colors: [u8; 2],
}

impl Board {
    pub fn get_piece_64(&self, square_index: usize) -> u8 {
        self.squares[square_index]
    }

    pub fn write_piece(&mut self, piece: u8, square_index: usize) {
        let bit_square = BIT_SQUARES[square_index];
        if piece == PIECE_NONE {
            let old_piece = self.squares[square_index];
            if old_piece != PIECE_NONE {
                let side = if old_piece & COLOR_BLACK != 0 { 1 } else { 0 };
                let piece_type = (old_piece & PIECE_MASK) as usize;
                self.piece_bitboards[side][piece_type] &= !bit_square;
                self.side_occupancy[side] &= !bit_square;
                self.occupancy &= !bit_square;
                self.piecesquare_midgame -= PIECE_SQUARE_TABLES[side][piece_type - 1][square_index];
                self.piecesquare_endgame -= PIECE_SQUARE_TABLES[side][piece_type - 1 + 6][square_index];
            }
        } else {
            let side = if piece & COLOR_BLACK != 0 { 1 } else { 0 };
            let piece_type = (piece & PIECE_MASK) as usize;
            self.piece_bitboards[side][piece_type] |= bit_square;
            self.side_occupancy[side] |= bit_square;
            self.occupancy |= bit_square;
            self.piecesquare_midgame += PIECE_SQUARE_TABLES[side][piece_type - 1][square_index];
            self.piecesquare_endgame += PIECE_SQUARE_TABLES[side][piece_type - 1 + 6][square_index];
        }

        self.squares[square_index] = piece;
    }

    pub fn from_fen(fen: &str) -> Result<Board, String> {
        if !fen.is_ascii() {
            return Err(String::from("Expected FEN to only contain ASCII characters"));
        }

        let fen_pieces: Vec<&str> = fen.split(' ').collect();
        if fen_pieces.len() < 4 || fen_pieces.len() > 6 {
            return Err(format!(
                "Expected FEN to have 4 to 6 space-delimited parts but it had {}",
                fen_pieces.len()
            ));
        }

        let mut board = Board {
            squares: [0; 64],
            white_to_move: true,
            castling_rights: 0,
            en_passant_target_square_index: None,
            halfmove_clock: 0,
            fullmove_counter: 1,
            hash: 0,
            game_stage: 0,
            repetitions: RepetitionTracker::default(),
            piece_counts: [[0; 7]; 2],
            piece_bitboards: [[0; 7]; 2],
            side_occupancy: [0; 2],
            occupancy: 0,
            piecesquare_midgame: 0,
            piecesquare_endgame: 0,
            bishop_colors: [0; 2],
        };
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
                    board.bishop_colors[0] |= if BIT_SQUARES[board_index] & DARK_SQUARES != 0 {
                        BISHOP_COLORS_DARK
                    } else {
                        BISHOP_COLORS_LIGHT
                    };
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
                    board.bishop_colors[1] |= if BIT_SQUARES[board_index] & DARK_SQUARES != 0 {
                        BISHOP_COLORS_DARK
                    } else {
                        BISHOP_COLORS_LIGHT
                    };
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
                    ep_square_index += 8 * (chars[1].to_digit(10).unwrap() as u8 - 1);
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

        if fen_pieces.len() > 4 {
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

            if fen_pieces.len() > 5 {
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
            }
        }

        board.repetitions.add_start_position(board.hash);

        Ok(board)
    }

    pub fn pretty_print(&self) -> String {
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

    pub fn to_fen(&self) -> String {
        let mut result = String::new();

        // Pieces on the board
        let mut board_index = 56;
        for rank_count in 1..=8 {
            let mut empty_count = 0;
            for file_count in 1..=8 {
                let piece = self.get_piece_64(board_index);

                if piece != PIECE_NONE {
                    if empty_count > 0 {
                        result += empty_count.to_string().as_str();
                        empty_count = 0;
                    }

                    result += piece_to_name(piece).to_string().as_str();
                } else {
                    empty_count += 1;

                    if file_count == 8 {
                        result += empty_count.to_string().as_str();
                        empty_count = 0;
                    }
                }

                board_index += 1;
            }

            if rank_count != 8 {
                result += "/";
                board_index -= 16;
            }
        }

        // Side to move
        if self.white_to_move {
            result += " w ";
        } else {
            result += " b ";
        }

        // Castling
        if self.castling_rights != 0 {
            if self.castling_rights & CASTLE_WHITE_KING_FLAG != 0 {
                result += "K";
            }

            if self.castling_rights & CASTLE_WHITE_QUEEN_FLAG != 0 {
                result += "Q";
            }

            if self.castling_rights & CASTLE_BLACK_KING_FLAG != 0 {
                result += "k";
            }

            if self.castling_rights & CASTLE_BLACK_QUEEN_FLAG != 0 {
                result += "q";
            }

            result += " ";
        } else {
            result += "- ";
        }

        // En passant
        if let Some(ep_index) = self.en_passant_target_square_index {
            let rank = rank_8x8(ep_index);
            let file = file_8x8(ep_index);

            result += format!("{}{} ", (b'a' + file) as char, rank).as_str();
        } else {
            result += "- ";
        }

        // Half move clock
        result += self.halfmove_clock.to_string().as_str();
        result += " ";

        // Full move count
        result += self.fullmove_counter.to_string().as_str();

        result
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
            .field("game_stage", &self.game_stage)
            .field("repetitions", &self.repetitions)
            .field("piece_counts", &self.piece_counts)
            .field("piece_bitboards", &"See end value")
            .field("side_occupancy", &"See end value")
            .field("occupancy", &"See end value")
            .field("bishop_colors", &self.bishop_colors)
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
        writeln!(f, "pretty version:\n{}", self.pretty_print()).unwrap();

        write!(f, "Piece bitboards:\nWhite:").unwrap();
        self.piece_bitboards[0]
            .iter()
            .skip(1)
            .for_each(|v| writeln!(f, "{}", pretty_print_bitboard(*v)).unwrap());
        write!(f, "Black:").unwrap();
        self.piece_bitboards[1]
            .iter()
            .skip(1)
            .for_each(|v| writeln!(f, "{}", pretty_print_bitboard(*v)).unwrap());

        write!(f, "Side occupancy:\nWhite:").unwrap();
        writeln!(f, "{}", pretty_print_bitboard(self.side_occupancy[0])).unwrap();
        write!(f, "Black:").unwrap();
        writeln!(f, "{}", pretty_print_bitboard(self.side_occupancy[1])).unwrap();

        writeln!(f, "Occupancy:{}", pretty_print_bitboard(self.occupancy)).unwrap();
        writeln!(f, "FEN: {}", self.to_fen())
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
    board.game_stage += GAME_STAGE_VALUES[piece_code as usize];
    board.piece_counts[if white { 0 } else { 1 }][piece_code as usize] += 1;
}
