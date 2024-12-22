pub const MOVE_FLAG_PROMOTION: u16 = 1 << 12;
pub const MOVE_FLAG_CAPTURE: u16 = 1 << 12;

pub const MOVE_DOUBLE_PAWN: u16 = 1;
pub const MOVE_KING_CASTLE: u16 = 2;
pub const MOVE_QUEEN_CASTLE: u16 = 3;
pub const MOVE_PROMO_KNIGHT: u16 = MOVE_FLAG_PROMOTION;
pub const MOVE_PROMO_BISHOP: u16 = MOVE_FLAG_PROMOTION | 1;
pub const MOVE_PROMO_ROOK: u16 = MOVE_FLAG_PROMOTION | 2;
pub const MOVE_PROMO_QUEEN: u16 = MOVE_FLAG_PROMOTION | 3;

#[derive(Debug)]
pub struct Move {
    // from: 6 bits, to: 6 bits: flags: 4 bits. Using flags format from https://www.chessprogramming.org/Encoding_Moves
    pub data: u16,
}

impl Move {
    pub fn new(from_square_index: u8, to_square_index: u8, flags: u16) -> Move {
        Move {
            data: from_square_index as u16 | ((to_square_index as u16) << 6) | (flags << 12),
        }
    }
}

#[derive(Debug)]
pub struct MoveRollback {
    // Only added when a piece is actually captured
    pub captured_pieces: Vec<u8>,
    pub ep_index: Vec<u8>,
    pub castling_rights: Vec<u8>,
    pub halfmove_clocks: Vec<u8>,
}
