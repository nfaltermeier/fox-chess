use std::fmt::{format, Debug};

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
// little endian file-rank mapping https://www.chessprogramming.org/Square_Mapping_Considerations
static BOARD_SQUARE_INDEX_TRANSLATION_64: [u8; 64] = [
    21, 22, 23, 24, 25, 26, 27, 28,
    31, 32, 33, 34, 35, 36, 37, 38,
    41, 42, 43, 44, 45, 46, 47, 48,
    51, 52, 53, 54, 55, 56, 57, 58,
    61, 62, 63, 64, 65, 66, 67, 68,
    71, 72, 73, 74, 75, 76, 77, 78,
    81, 82, 83, 84, 85, 86, 87, 88,
    91, 92, 93, 94, 95, 96, 97, 98
];

const PIECE_NONE: u8 = 0;
const PIECE_PAWN: u8 = 0x1;
const PIECE_KNIGHT: u8 = 0x2;
const PIECE_BISHOP: u8 = 0x3;
const PIECE_ROOK: u8 = 0x4;
const PIECE_QUEEN: u8 = 0x5;
const PIECE_KING: u8 = 0x6;

const COLOR_BLACK: u8 = 1 << 3;

const CASTLE_WHITE_KING: u8 = 1;
const CASTLE_WHITE_QUEEN: u8 = 1 << 1;
const CASTLE_BLACK_KING: u8 = 1 << 2;
const CASTLE_BLACK_QUEEN: u8 = 1 << 3;

pub struct Board {
    squares: [u8; 120],
    pub white_to_move: bool,
    pub castling_rights: u8,
    pub en_passant_target_square_index: Option<u8>,
    pub halfmove_clock: u8,
    // Is this needed?
    pub fullmove_counter: u16,
}

impl Board {
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
                    board.write_piece(PIECE_PAWN, board_index);
                    board_index += 1;
                }
                'N' => {
                    board.write_piece(PIECE_KNIGHT, board_index);
                    board_index += 1;
                }
                'B' => {
                    board.write_piece(PIECE_BISHOP, board_index);
                    board_index += 1;
                }
                'R' => {
                    board.write_piece(PIECE_ROOK, board_index);
                    board_index += 1;
                }
                'Q' => {
                    board.write_piece(PIECE_QUEEN, board_index);
                    board_index += 1;
                }
                'K' => {
                    board.write_piece(PIECE_KING, board_index);
                    board_index += 1;
                }
                'p' => {
                    board.write_piece(PIECE_PAWN | COLOR_BLACK, board_index);
                    board_index += 1;
                }
                'n' => {
                    board.write_piece(PIECE_KNIGHT | COLOR_BLACK, board_index);
                    board_index += 1;
                }
                'b' => {
                    board.write_piece(PIECE_BISHOP | COLOR_BLACK, board_index);
                    board_index += 1;
                }
                'r' => {
                    board.write_piece(PIECE_ROOK | COLOR_BLACK, board_index);
                    board_index += 1;
                }
                'q' => {
                    board.write_piece(PIECE_QUEEN | COLOR_BLACK, board_index);
                    board_index += 1;
                }
                'k' => {
                    board.write_piece(PIECE_KING | COLOR_BLACK, board_index);
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
        } else {
            return Err(format!("Encountered unexpected Side to move value '{}'", fen_pieces[1]));
        }

        if fen_pieces[2] != "-" {
            for c in fen_pieces[2].chars() {
                match c {
                    'K' => {
                        board.castling_rights |= CASTLE_WHITE_KING;
                    }
                    'Q' => {
                        board.castling_rights |= CASTLE_WHITE_QUEEN;
                    }
                    'k' => {
                        board.castling_rights |= CASTLE_BLACK_KING;
                    }
                    'q' => {
                        board.castling_rights |= CASTLE_BLACK_QUEEN;
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

            let mut ep_square_index: u8 = 0;
            match chars[0] {
                'a'..='h' => {
                    ep_square_index += chars[0] as u8 - 'a' as u8;
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

    pub fn write_piece(&mut self, piece: u8, square_index: usize) {
        self.squares[BOARD_SQUARE_INDEX_TRANSLATION_64[square_index] as usize] = piece;
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

        writeln!(f, "\nsquares: \n{}", pretty_squares)
    }
}
