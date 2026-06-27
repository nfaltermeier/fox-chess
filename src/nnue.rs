use crate::{
    bitboard::bitscan_forward_and_reset,
    board::{Board, COLOR_BLACK, PIECE_KING, PIECE_MASK},
};

const QA: i32 = 255;
const QB: i32 = 64;
const SCALE: i32 = 400;
const HIDDEN_SIZE: usize = 32;

// Based on the bullet inference example

// Find the network files at https://github.com/nfaltermeier/fox-chess-nets/releases
pub static NNUE: Network = unsafe { std::mem::transmute(*include_bytes!("../networks/fennec.nnue")) };

#[inline]
/// Square Clipped ReLU - Activation Function.
/// Note that this takes the i16s in the accumulator to i32s.
/// Range is 0.0 .. 1.0 (in other words, 0 to QA*QA quantized).
fn screlu(x: i16) -> i32 {
    let y = i32::from(x).clamp(0, QA);
    y * y
}

/// This is the quantised format that bullet outputs.
#[repr(C)]
pub struct Network {
    /// Column-Major `HIDDEN_SIZE x 768` matrix.
    /// Values have quantization of QA.
    feature_weights: [Accumulator; 768],
    /// Vector with dimension `HIDDEN_SIZE`.
    /// Values have quantization of QA.
    feature_bias: Accumulator,
    /// Column-Major `1 x (2 * HIDDEN_SIZE)`
    /// matrix, we use it like this to make the
    /// code nicer in `Network::evaluate`.
    /// Values have quantization of QB.
    output_weights: [i16; 2 * HIDDEN_SIZE],
    /// Scalar output bias.
    /// Value has quantization of QA * QB.
    output_bias: i16,
}

impl Network {
    /// Calculates the output of the network, starting from the already
    /// calculated hidden layer (done efficiently during makemoves).
    pub fn evaluate(&self, us: &Accumulator, them: &Accumulator) -> i16 {
        // Initialise output.
        let mut output = 0;

        // Side-To-Move Accumulator -> Output.
        for (&input, &weight) in us.vals.iter().zip(&self.output_weights[..HIDDEN_SIZE]) {
            output += screlu(input) * i32::from(weight);
        }

        // Not-Side-To-Move Accumulator -> Output.
        for (&input, &weight) in them.vals.iter().zip(&self.output_weights[HIDDEN_SIZE..]) {
            output += screlu(input) * i32::from(weight);
        }

        // Reduce quantization from QA * QA * QB to QA * QB.
        output /= QA;

        // Add bias.
        output += i32::from(self.output_bias);

        // Aptarget_ply eval scale.
        output *= SCALE;

        // Remove quantisation altogether.
        output /= QA * QB;

        output as i16
    }
}

/// A column of the feature-weights matrix.
/// Note the `align(64)`.
#[derive(Clone, PartialEq)]
#[repr(C, align(64))]
pub struct Accumulator {
    vals: [i16; HIDDEN_SIZE],
}

impl Accumulator {
    /// Initialised with bias so we can just efficiently
    /// operate on it afterwards.
    pub fn new(net: &Network) -> Self {
        net.feature_bias.clone()
    }

    /// Add a feature to an accumulator.
    pub fn add_feature(&mut self, feature_idx: usize, net: &Network) {
        for (i, d) in self.vals.iter_mut().zip(&net.feature_weights[feature_idx].vals) {
            *i += *d
        }
    }

    // Fused updates based on https://asteri.sm/files/2024-06-01-nnue#fused-updates
    pub fn add1_sub1(
        input: &Accumulator,
        output: &mut Accumulator,
        net: &Network,
        add_index: usize,
        remove_index: usize,
    ) {
        let add_weights = &net.feature_weights[add_index];
        let remove_weights = &net.feature_weights[remove_index];

        for i in 0..HIDDEN_SIZE {
            output.vals[i] = input.vals[i] + add_weights.vals[i] - remove_weights.vals[i];
        }
    }

    pub fn add1_sub2(
        input: &Accumulator,
        output: &mut Accumulator,
        net: &Network,
        add_index: usize,
        remove_index1: usize,
        remove_index2: usize,
    ) {
        let add_weights = &net.feature_weights[add_index];
        let remove_weights1 = &net.feature_weights[remove_index1];
        let remove_weights2 = &net.feature_weights[remove_index2];

        for i in 0..HIDDEN_SIZE {
            output.vals[i] = input.vals[i] + add_weights.vals[i] - remove_weights1.vals[i] - remove_weights2.vals[i];
        }
    }

    pub fn add2_sub2(
        input: &Accumulator,
        output: &mut Accumulator,
        net: &Network,
        add_index1: usize,
        add_index2: usize,
        remove_index1: usize,
        remove_index2: usize,
    ) {
        let add_weights1 = &net.feature_weights[add_index1];
        let add_weights2 = &net.feature_weights[add_index2];
        let remove_weights1 = &net.feature_weights[remove_index1];
        let remove_weights2 = &net.feature_weights[remove_index2];

        for i in 0..HIDDEN_SIZE {
            output.vals[i] = input.vals[i] + add_weights1.vals[i] + add_weights2.vals[i]
                - remove_weights1.vals[i]
                - remove_weights2.vals[i];
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct AccumulatorPair {
    pub white: Accumulator,
    pub black: Accumulator,
}

impl AccumulatorPair {
    pub fn from(board: &Board, net: &Network) -> Self {
        let mut result = AccumulatorPair {
            white: Accumulator::new(net),
            black: Accumulator::new(net),
        };

        let mut pieces = board.occupancy;
        while pieces != 0 {
            let index = bitscan_forward_and_reset(&mut pieces) as usize;

            let piece = board.get_piece_64(index);
            let piece_type = (piece & PIECE_MASK) as usize;
            let white = piece & COLOR_BLACK == 0;

            result
                .white
                .add_feature((piece_type - 1 + if white { 0 } else { 6 }) * 64 + index, net);
            // square index flipped horizontally and vertically
            result
                .black
                .add_feature(((piece_type - 1 + if white { 6 } else { 0 }) * 64 + index) ^ 56, net);
        }

        result
    }
}

pub struct AccumulatorPairStack {
    accumulators: Vec<AccumulatorPair>,
    ply: u16,
}

impl AccumulatorPairStack {
    pub fn new() -> Self {
        Self {
            accumulators: Vec::with_capacity(256),
            ply: 0,
        }
    }

    pub fn get_current_accumulator(&self) -> &AccumulatorPair {
        self.accumulators.get(self.ply as usize).unwrap()
    }

    pub fn init(&mut self, board: &Board, net: &Network) {
        self.accumulators.clear();
        self.accumulators.push(AccumulatorPair::from(board, net));
        self.ply = 0;
    }

    pub fn add1_sub1(&mut self, net: &Network, add: PieceOnSquare, remove: PieceOnSquare) {
        self.ply += 1;
        let (input, output) = self.get_accumulator_pairs_for_update(self.ply);

        Accumulator::add1_sub1(
            &input.white,
            &mut output.white,
            net,
            add.get_index(true),
            remove.get_index(true),
        );
        Accumulator::add1_sub1(
            &input.black,
            &mut output.black,
            net,
            add.get_index(false),
            remove.get_index(false),
        );
    }

    pub fn add1_sub2(&mut self, net: &Network, add: PieceOnSquare, remove1: PieceOnSquare, remove2: PieceOnSquare) {
        self.ply += 1;
        let (input, output) = self.get_accumulator_pairs_for_update(self.ply);

        Accumulator::add1_sub2(
            &input.white,
            &mut output.white,
            net,
            add.get_index(true),
            remove1.get_index(true),
            remove2.get_index(true),
        );
        Accumulator::add1_sub2(
            &input.black,
            &mut output.black,
            net,
            add.get_index(false),
            remove1.get_index(false),
            remove2.get_index(false),
        );
    }

    pub fn add2_sub2(
        &mut self,
        net: &Network,
        add1: PieceOnSquare,
        add2: PieceOnSquare,
        remove1: PieceOnSquare,
        remove2: PieceOnSquare,
    ) {
        self.ply += 1;
        let (input, output) = self.get_accumulator_pairs_for_update(self.ply);

        Accumulator::add2_sub2(
            &input.white,
            &mut output.white,
            net,
            add1.get_index(true),
            add2.get_index(true),
            remove1.get_index(true),
            remove2.get_index(true),
        );
        Accumulator::add2_sub2(
            &input.black,
            &mut output.black,
            net,
            add1.get_index(false),
            add2.get_index(false),
            remove1.get_index(false),
            remove2.get_index(false),
        );
    }

    /// For when the search goes back to an earlier position, make the previous accumulator the active one
    pub fn decr_ply(&mut self) {
        self.ply -= 1;
    }

    fn get_accumulator_pairs_for_update(&mut self, target_ply: u16) -> (&AccumulatorPair, &mut AccumulatorPair) {
        // target_ply 0 is the baseline correct accumulator value, it should not be incrementally updated
        debug_assert_ne!(target_ply, 0);

        if target_ply as usize == self.accumulators.len() {
            if self.accumulators.capacity() == self.accumulators.len() {
                self.accumulators.reserve(1);
            }

            let new_len = self.accumulators.len() + 1;
            // Safety: accumulators has at least one extra capacity and an accumulator is just an array of ints, so any bitpattern is valid.
            // These uninitialized bytes are expected to be written to in add1_sub1, add1_sub2, or add2_sub2 before the caller returns.
            unsafe {
                self.accumulators.set_len(new_len);
            }
        }

        let (front, back) = self.accumulators.split_at_mut(target_ply as usize);
        let source = front.last().unwrap();
        let target = back.first_mut().unwrap();

        (source, target)
    }
}

#[derive(Clone, Copy)]
pub struct PieceOnSquare {
    /// One is subtracted from my typical piece type values, such that pawn becomes 0
    piece_type_sub_one: u8,
    white: bool,
    square: u8,
}

impl PieceOnSquare {
    /// Normal piece type values should be passed in, where pawn is 1 and none is 0
    pub fn new(piece_type: u8, white: bool, square: u8) -> Self {
        debug_assert_ne!(piece_type, 0);
        debug_assert!(
            piece_type <= PIECE_KING,
            "unexpected piece_type value {piece_type} received in PieceOnSquare::new"
        );
        debug_assert!(
            square <= 63,
            "unexpected square value {square} received in PieceOnSquare::new"
        );

        Self {
            piece_type_sub_one: piece_type - 1,
            white,
            square,
        }
    }

    fn get_index(self, white_pov: bool) -> usize {
        let pov_square = if white_pov { self.square } else { self.square ^ 56 };
        (self.piece_type_sub_one as usize + if white_pov ^ self.white { 6 } else { 0 }) * 64 + pov_square as usize
    }
}
