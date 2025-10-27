use std::{sync::mpsc, time::Instant};

use vampirc_uci::UciSearchControl;

use crate::{
    board::Board,
    search::{DEFAULT_HISTORY_TABLE, Searcher},
    transposition_table::TranspositionTable,
};

pub fn bench() {
    let bench_fens = [
        "rn3rk1/ppp1ppbp/6p1/3qP3/3P2bP/4BN2/PP2BPP1/R2QK2R b KQ - 1 11",
        "r6k/1p3p2/1np1b1pp/p2pP2n/P2P3q/1P2P2P/3QN1BK/2R2R2 b - - 1 26",
        "4rk2/pp5R/2p1pb2/2n5/1K1PB1Q1/2N1P2R/PP3rP1/5q2 w - - 7 28",
        "r3r1k1/1b1q1p2/p5pp/1p1pP1b1/2pP1N2/P3PQ1P/1PB3P1/2R2R1K w - - 6 28",
        "r7/p2k3p/1p2pp2/1P6/3P1nP1/R4N1P/5P1K/8 b - - 1 40",
        "2r1r1k1/5p2/4q2Q/p2p1p2/3Pn3/4P2P/5PPB/1R1R2K1 w - - 1 32",
        "2kr3r/p1p2pp1/2nqpn1p/8/3Pb3/BQ2PNP1/P4PBP/R4RK1 b - - 4 17",
        "2qr3k/5p1p/2B2Pp1/1P6/3p2n1/1P1Qb1P1/r6P/1RB1R1K1 w - - 4 28",
        "r1bqk2r/1ppn1pp1/p2b1n1p/3p4/3P4/2N1BNP1/PP2PPBP/R2QK2R b KQkq - 1 9",
        "7r/1bk3R1/5p2/2p1pP1P/4P3/P1P2QPK/1q6/8 b - - 3 41",
        "3r2k1/1p3rpp/pq2p3/1b3p2/nP2Q3/P4NPB/4PP1P/2R1R1K1 w - - 0 23",
        "8/8/5pk1/2R5/6P1/7r/5K2/8 b - - 18 60",
        "8/1n1n1p2/5k1p/p7/1BP2P1P/4K3/2B5/8 w - - 0 54",
        "3r1rk1/1pqb1pb1/p2ppnpp/n1pP4/P1P1PP2/1PN3PP/2Q1N1BK/R1B2R2 w - - 0 16",
        "2r4r/3knpp1/p3p3/1p1pP3/2P4p/PP6/3N1PPP/3R1RK1 w - b6 0 22",
        "r1bqkbnr/pp2pppp/2n5/2pp4/3P4/2P1PN2/PP3PPP/RNBQKB1R b KQkq - 0 4",
        "2r5/5pk1/2n1pnp1/2Qp3p/8/4NBPP/1q2PP2/3R2K1 b - - 5 43",
        "2r1r1k1/2p1npb1/p4q2/NpRPp2b/4P1p1/P3RN2/1P3PP1/1B4KQ b - - 1 30",
        "r1bqr1k1/pppn1pbp/3p2p1/4P3/6n1/1P3NP1/PBPNPPBP/R2Q1RK1 w - - 1 10",
        "1q4k1/1p4p1/4p3/P2pp2p/1P2P1nP/2BP1KP1/4Q3/8 b - b3 0 32",
        "r3k2r/1p2bppp/pqnpbn2/8/3NPB2/2N5/PPPQB1PP/2KR3R b kq - 6 12",
        "r2qr1k1/ppQ2pp1/7p/3Nn3/3pB1b1/8/PPP2PPP/R3R1K1 b - - 0 19",
        "1rq1kb1r/pb3ppp/2nppn2/2p5/2P1P3/2N2NP1/P2P1PBP/1RBQ1RK1 w k - 0 11",
        "6k1/p4p1p/2B1np2/1p2r3/1P6/2PR2PP/5K2/8 b - - 1 29",
        "r3k2r/1b1p1ppp/pp6/2qPp3/4P3/P2QP3/1P2B1PP/R3K2R w KQkq - 3 16",
        "8/1pp1k1Br/p3p2p/2pb3N/6P1/1P4K1/P7/2R5 b - - 1 31",
        "3r1r1k/1bq2ppp/pppb1n2/4pP2/2P1P3/P1N1B3/1P2BQPP/3R1RK1 b - - 6 16",
        "r1bqk2r/pp3pbp/2n2np1/1B1p2B1/3P4/P1N2N2/1P3PPP/R2QK2R b KQkq - 3 10",
        "2kr1b1r/ppp1ppp1/2n1q3/8/3PP2p/2PQ1N2/PP4P1/R3KB1R w KQ - 1 13",
        "3r2k1/5p2/p3rp2/1pnp3N/b4P2/2PBP3/P2K2PP/1R5R w - - 1 24",
        "1r4k1/1ppb1p2/3p1Qpp/bP1P4/8/6PP/2R2PBK/4r3 w - - 2 40",
        "8/1p6/6K1/P1p4p/7P/1n1k4/5P2/4B3 w - - 0 57",
        "2rq1rk1/1b2bpp1/1pn1pn1p/p2p4/P2P4/2PB1N1P/1P1NQPPB/3R1RK1 b - - 1 15",
        "rn3rk1/7p/p4p2/1p3q2/4N3/4Q3/4B2P/1R2KR2 b - - 1 24",
        "3rr1k1/pp3bp1/2p4p/q2n1Q2/3Pp3/1P5P/3NBPP1/3RR1K1 b - - 3 30",
        "8/1pbr4/2p2p1k/2Pr1p1p/P2P1p1N/6PR/4R1KP/8 w - - 6 42",
        "2rqkb1r/1p1b1ppp/pQnppn2/8/4P3/2N1BN2/PPP1BPPP/R4RK1 b k - 3 14",
        "r3k2r/pp2bppp/2b1p3/2PpP3/1NqP4/R4N2/1P1Q1PPP/5RK1 b k - 7 18",
        "r1bq1rk1/p4ppp/n1p1p3/2p1P3/3P4/PPN2NP1/5P1P/R2Q1RK1 b - - 3 17",
        "6k1/p4pp1/2b5/1p5p/3P1P1P/P1Pp2K1/1R1B2P1/3r4 w - - 2 32",
        "3R4/8/2knp3/4n3/6p1/4P1P1/4KP2/8 w - - 46 73",
        "r5k1/1R3pp1/2p1rb1p/3p3P/3P1BP1/p3PB2/Q4P2/2q2NK1 b - - 1 32",
        "3r1rk1/1pbn1p1p/1q4p1/p1p1p3/Pn1pPN2/1P1P2PP/1BPQ1PB1/R1R3K1 w - - 0 24",
        "rb2r1k1/pp1b1ppp/5q2/3p4/3N2n1/1QP1B1P1/PP2RP1P/RN4K1 b - - 4 19",
        "3r4/4r1pk/1p2p2p/8/2P3Q1/pP4PK/P1q1R2P/R7 b - - 15 37",
        "r7/2q1ppk1/1pp1b1pp/p1prP3/P2n2P1/3PQ2P/1PPNNP1K/R1R5 w - - 4 23",
        "2rq2nr/1p1k1Ppp/p3p3/3BP3/3b2P1/2N1n3/PPP4P/R2QK2R w KQ - 0 16",
        "2kr3r/1bpnqp2/p3pp1p/1p6/P2P4/2Q1NP2/1P2P1PP/2KR1B1R b - - 0 16",
        "8/5pk1/4p1b1/2R1P1p1/1p2P1Pp/pP1q1P1P/P1R3BK/8 b - - 3 45",
        "4r1k1/4rpp1/Q2p2q1/p1p2RP1/4P2p/1P1P3P/P5K1/5R2 w - - 1 33",
    ];

    let tc = None;
    let sc = Some(UciSearchControl::depth(9));

    let mut nodes = 0;
    let start_time = Instant::now();

    for fen in bench_fens {
        let mut board = Board::from_fen(fen).unwrap();

        let mut transposition_table = TranspositionTable::new(18);
        let mut history = DEFAULT_HISTORY_TABLE;
        let mut continuation_history = [[[[[0; 64]; 6]; 64]; 6]; 2];

        let (_, stop_rx) = mpsc::channel::<()>();
        let mut searcher = Searcher::new(
            &mut board,
            &mut transposition_table,
            &mut history,
            &stop_rx,
            &mut continuation_history,
        );

        searcher.iterative_deepening_search(&tc, &sc);

        nodes += searcher.stats.total_nodes;
    }

    let elapsed = start_time.elapsed();

    let nps = nodes as f64 / elapsed.as_secs_f64();

    println!("Nodes: {nodes} NPS: {nps:.0} Time: {elapsed:#?}");
}
