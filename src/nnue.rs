use std::ptr;

use crate::{
    bitboard::bitscan_forward_and_reset,
    board::{Board, COLOR_BLACK, PIECE_KING, PIECE_MASK},
};

const QA: i16 = 255;
const QB: i32 = 64;
const SCALE: i32 = 400;
const HIDDEN_SIZE: usize = 512;
const OUTPUT_BUCKET_COUNT: usize = 8;

// Based on the bullet inference example

// Network files will be downloaded by the build script and NEURAL_NETWORK env variable will be set by the build script.
pub static NNUE: Network = unsafe { std::mem::transmute(*include_bytes!(concat!("../", env!("NEURAL_NETWORK")))) };

#[inline]
/// Square Clipped ReLU - Activation Function.
/// Range is 0.0 .. 1.0 (in other words, 0 to QA*QA quantized).
/// Implements Lizard screlu as described by https://asteri.sm/files/2024-06-01-nnue#lizard-simd-for-squared-clipped-relu
/// With notes based on their description
fn screlu_mul(x: i16, w: i16) -> i32 {
    let clamped = x.clamp(0, QA);
    // Will not overflow because max(clamped) = 255 [QA] and max(w) = 127
    // because weights are clamped to [-1.98, 1.98] by the optimizer. round(1.98 * 64 [QB]) = 127.
    let intermediate = clamped * w;
    intermediate as i32 * clamped as i32
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
    /// Column-Major `OUTPUT_BUCKET_COUNT x (2 * HIDDEN_SIZE)`
    /// matrix, we use it like this to make the
    /// code nicer in `Network::evaluate`.
    /// Values have quantization of QB.
    output_weights: [[i16; 2 * HIDDEN_SIZE]; OUTPUT_BUCKET_COUNT],
    /// Scalar output bias.
    /// Value has quantization of QA * QB.
    output_biases: [i16; OUTPUT_BUCKET_COUNT],
}

impl Network {
    /// Calculates the output of the network, starting from the already
    /// calculated hidden layer (done efficiently during makemoves).
    #[inline]
    pub fn evaluate(&self, us: &Accumulator, them: &Accumulator, board: &Board) -> i16 {
        // Initialise output.
        let mut output = 0;

        let output_bucket_index =
            (board.occupancy.count_ones().min(32) as usize - 2) / (32usize.div_ceil(OUTPUT_BUCKET_COUNT));

        let bucket_output_weights = &self.output_weights[output_bucket_index];

        // Side-To-Move Accumulator -> Output.
        for (&input, &weight) in us.vals.iter().zip(&bucket_output_weights[..HIDDEN_SIZE]) {
            output += screlu_mul(input, weight);
        }

        // Not-Side-To-Move Accumulator -> Output.
        for (&input, &weight) in them.vals.iter().zip(&bucket_output_weights[HIDDEN_SIZE..]) {
            output += screlu_mul(input, weight);
        }

        // Reduce quantization from QA * QA * QB to QA * QB.
        output /= QA as i32;

        // Add bias.
        output += i32::from(self.output_biases[output_bucket_index]);

        // Aptarget_ply eval scale.
        output *= SCALE;

        // Remove quantisation altogether.
        output /= QA as i32 * QB;

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

    fn from(board: &Board, net: &Network, white_pov: bool, orientation: Orientation) -> Self {
        let mut result = Self::new(net);

        let mut pieces = board.occupancy;
        while pieces != 0 {
            let index = bitscan_forward_and_reset(&mut pieces) as usize;

            let piece = board.get_piece_64(index);
            let piece_type = (piece & PIECE_MASK) as usize;
            let white = piece & COLOR_BLACK == 0;

            if white_pov {
                result.add_feature(
                    (piece_type - 1 + if white { 0 } else { 6 }) * 64 + orientation.transform_square(index),
                    net,
                );
            } else {
                // square index flipped horizontally and vertically
                result.add_feature(
                    ((piece_type - 1 + if white { 6 } else { 0 }) * 64 + orientation.transform_square(index)) ^ 56,
                    net,
                );
            }
        }

        result
    }
}

// Safety Note: all fields with bit pattern requirements must be initialized in AccumulatorPairStack::get_accumulator_pairs_for_update
#[derive(Clone, PartialEq)]
pub struct AccumulatorPair {
    pub white: Accumulator,
    white_orientation: Orientation,
    pub black: Accumulator,
    black_orientation: Orientation,
}

impl AccumulatorPair {
    pub fn from(board: &Board, net: &Network) -> Self {
        let orientations = Orientation::from(board);

        AccumulatorPair {
            white: Accumulator::from(board, net, true, orientations.0),
            white_orientation: orientations.0,
            black: Accumulator::from(board, net, false, orientations.1),
            black_orientation: orientations.1,
        }
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

    pub fn add1_sub1(&mut self, net: &Network, board: &Board, add: PieceOnSquare, remove: PieceOnSquare) {
        self.ply += 1;
        let (input, output) = self.get_accumulator_pairs_for_update(self.ply);

        let new_orientations = Orientation::from(board);

        if new_orientations.0 == input.white_orientation {
            Accumulator::add1_sub1(
                &input.white,
                &mut output.white,
                net,
                add.get_index(true, input.white_orientation),
                remove.get_index(true, input.white_orientation),
            );
        } else {
            output.white = Accumulator::from(board, net, true, new_orientations.0);
        }
        output.white_orientation = new_orientations.0;

        if new_orientations.1 == input.black_orientation {
            Accumulator::add1_sub1(
                &input.black,
                &mut output.black,
                net,
                add.get_index(false, input.black_orientation),
                remove.get_index(false, input.black_orientation),
            );
        } else {
            output.black = Accumulator::from(board, net, false, new_orientations.1);
        }
        output.black_orientation = new_orientations.1;
    }

    pub fn add1_sub2(
        &mut self,
        net: &Network,
        board: &Board,
        add: PieceOnSquare,
        remove1: PieceOnSquare,
        remove2: PieceOnSquare,
    ) {
        self.ply += 1;
        let (input, output) = self.get_accumulator_pairs_for_update(self.ply);

        let new_orientations = Orientation::from(board);

        if new_orientations.0 == input.white_orientation {
            Accumulator::add1_sub2(
                &input.white,
                &mut output.white,
                net,
                add.get_index(true, input.white_orientation),
                remove1.get_index(true, input.white_orientation),
                remove2.get_index(true, input.white_orientation),
            );
        } else {
            output.white = Accumulator::from(board, net, true, new_orientations.0);
        }
        output.white_orientation = new_orientations.0;

        if new_orientations.1 == input.black_orientation {
            Accumulator::add1_sub2(
                &input.black,
                &mut output.black,
                net,
                add.get_index(false, input.black_orientation),
                remove1.get_index(false, input.black_orientation),
                remove2.get_index(false, input.black_orientation),
            );
        } else {
            output.black = Accumulator::from(board, net, false, new_orientations.1);
        }
        output.black_orientation = new_orientations.1;
    }

    pub fn add2_sub2(
        &mut self,
        net: &Network,
        board: &Board,
        add1: PieceOnSquare,
        add2: PieceOnSquare,
        remove1: PieceOnSquare,
        remove2: PieceOnSquare,
    ) {
        self.ply += 1;
        let (input, output) = self.get_accumulator_pairs_for_update(self.ply);

        let new_orientations = Orientation::from(board);

        if new_orientations.0 == input.white_orientation {
            Accumulator::add2_sub2(
                &input.white,
                &mut output.white,
                net,
                add1.get_index(true, input.white_orientation),
                add2.get_index(true, input.white_orientation),
                remove1.get_index(true, input.white_orientation),
                remove2.get_index(true, input.white_orientation),
            );
        } else {
            output.white = Accumulator::from(board, net, true, new_orientations.0);
        }
        output.white_orientation = new_orientations.0;

        if new_orientations.1 == input.black_orientation {
            Accumulator::add2_sub2(
                &input.black,
                &mut output.black,
                net,
                add1.get_index(false, input.black_orientation),
                add2.get_index(false, input.black_orientation),
                remove1.get_index(false, input.black_orientation),
                remove2.get_index(false, input.black_orientation),
            );
        } else {
            output.black = Accumulator::from(board, net, false, new_orientations.1);
        }
        output.black_orientation = new_orientations.1;
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

            let new_element_index = self.accumulators.len();
            let new_len = self.accumulators.len() + 1;
            // Safety: accumulators has at least one extra capacity and an accumulator is just an array of ints, so any bitpattern is valid.
            // The orientations are initialized to a default value, they contain fields that do not allow any bit pattern.
            // The uninitialized bytes in the white and black accumulators are expected to be written to in add1_sub1, add1_sub2, or add2_sub2 before the caller returns.
            unsafe {
                let new_element = self.accumulators.as_mut_ptr().add(new_element_index);

                let new_white_orientation = ptr::addr_of_mut!((*new_element).white_orientation);
                new_white_orientation.write(Orientation::default());

                let new_black_orientation = ptr::addr_of_mut!((*new_element).black_orientation);
                new_black_orientation.write(Orientation::default());

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

    fn get_index(self, white_pov: bool, orientation: Orientation) -> usize {
        let pov_square = orientation.transform_square_u8(if white_pov { self.square } else { self.square ^ 56 });
        (self.piece_type_sub_one as usize + if white_pov ^ self.white { 6 } else { 0 }) * 64 + pov_square as usize
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Default)]
struct Orientation {
    horiz_mirrored: bool,
}

impl Orientation {
    fn transform_square(self, square: usize) -> usize {
        square ^ if self.horiz_mirrored { 7 } else { 0 }
    }

    fn transform_square_u8(self, square: u8) -> u8 {
        self.transform_square(square as usize) as u8
    }

    /// Returns white then black orientation
    fn from(board: &Board) -> (Self, Self) {
        let white_mirror = board.piece_bitboards[0][PIECE_KING as usize].trailing_zeros() % 8 > 3;
        let black_mirror = board.piece_bitboards[1][PIECE_KING as usize].trailing_zeros() % 8 > 3;

        (
            Orientation {
                horiz_mirrored: white_mirror,
            },
            Orientation {
                horiz_mirrored: black_mirror,
            },
        )
    }
}
