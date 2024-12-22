pub const MAX_PAWNS: usize = 8;
pub const MAX_BISHOP: usize = 10;
pub const MAX_KNIGHT: usize = 10;
pub const MAX_ROOK: usize = 10;
pub const MAX_QUEEN: usize = 9;

#[derive(Default, Debug)]
pub struct Board {
    pub white: SideData,
    pub black: SideData,
}

#[derive(Default, Debug)]
pub struct SideData {
    pub num_pawns: usize,
    pub pawn_list: [u8; MAX_PAWNS],
    pub num_knights: usize,
    pub knight_list: [u8; MAX_KNIGHT],
    pub num_bishops: usize,
    pub bishop_list: [u8; MAX_BISHOP],
    pub num_rooks: usize,
    pub rook_list: [u8; MAX_ROOK],
    pub num_queens: usize,
    pub queen_list: [u8; MAX_QUEEN],
    pub king_location: u8,
}
