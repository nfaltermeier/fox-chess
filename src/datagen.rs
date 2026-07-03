use arrayvec::ArrayVec;
use clap::{Args, Subcommand};
use num_format::{Buffer, Locale};
use core::slice;
use rand::RngExt;
use serde::{Deserialize, Serialize};
use std::{
    fs::{self, File},
    io::{self, BufReader, BufWriter, Seek, SeekFrom, Write},
    mem,
    panic::{AssertUnwindSafe, catch_unwind},
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64, Ordering},
        mpsc,
    },
    thread::{self, sleep},
    time::{Duration, Instant},
};
use vampirc_uci::UciSearchControl;
use viriformat::{
    chess::{
        board::{Board as ViriBoard, DrawType, GameOutcome, WinType},
        chessmove::{Move as ViriMove, MoveFlags},
        piece::PieceType,
        types::Square,
    },
    dataformat::Game,
};

use crate::{
    STARTING_FEN,
    board::{Board, PIECE_MASK, PIECE_PAWN},
    evaluate::MATE_THRESHOLD,
    history::ThreadHistoryTables,
    moves::{MOVE_DOUBLE_PAWN, MOVE_EP_CAPTURE, MOVE_FLAG_CAPTURE, MOVE_KING_CASTLE, MOVE_QUEEN_CASTLE, Move},
    repetition_tracker::RepetitionTracker,
    search::{PrintMode, SearchResult, search_multithreaded, stats::SearchStats},
    staged_move_generator::StagedMoveGenerator,
    transposition_table::TranspositionTable,
    uci_required_options_helper::RequiredUciOptions,
};

#[derive(Subcommand)]
pub enum DatagenSubcommands {
    Start(DatagenArgs),
    Resume {
        #[arg(default_value_t = String::from("datagen"))]
        output_folder: String,
    },
}

const NO_EVAL: i16 = 32000;

#[derive(Args, Serialize, Deserialize, Clone)]
pub struct DatagenArgs {
    threads: u16,
    #[arg(long, default_value_t = String::from("datagen"))]
    output_folder: String,
    #[arg(long, default_value_t = 5000)]
    nodes: u64,
    #[arg(long, default_value_t = 1)]
    tt_size_mib: u32,
    #[arg(long, default_value_t = true)]
    soft_nodes: bool,
    #[arg(long, default_value_t = true)]
    merge_results: bool,
    #[arg(long, default_value_t = 4)]
    random_moves_per_side: u8,
    #[arg(long, default_value_t = 299)]
    maximum_opening_imbalance: i16,
    #[arg(long)]
    target_games: Option<u64>,
    /// Unfiltered
    #[arg(long)]
    target_positions: Option<u64>,
}

#[derive(Serialize, Deserialize, Default, Clone)]
pub struct ResumeData {
    games: u64,
    positions: u64,
    time: Duration,
    max_threads_used: u16,
}

#[derive(Serialize, Deserialize)]
struct SavedData {
    args: DatagenArgs,
    resume: ResumeData,
}

struct Sync {
    stop: AtomicBool,
    errored: AtomicBool,
    games: AtomicU64,
    positions: AtomicU64,
}

struct InitialDatagenStats {
    intial_positions: u64,
    intial_games: u64,
    time_from_previous_sessions: Duration,
    start_time: Instant,
}

pub fn start_new_datagen(args: &DatagenArgs) {
    start_datagen(args, None);
}

pub fn resume_datagen(output_folder: &str) {
    let config_file_contents = fs::read_to_string(format!("{output_folder}/datagen-config.json"))
        .expect("Failed to open read file (datagen-config.json) to resume");
    let mut saved_data = serde_json::from_str::<SavedData>(&config_file_contents)
        .expect("Failed to parse datagen-config.json contents as saved data");

    saved_data.args.output_folder = String::from(output_folder);

    start_datagen(&saved_data.args, Some(&saved_data.resume));
}

fn start_datagen(args: &DatagenArgs, resume: Option<&ResumeData>) {
    let mut save_data = SavedData {
        args: args.clone(),
        resume: resume.map_or(ResumeData::default(), |d| d.clone()),
    };

    let shared_stats = Arc::new(Sync {
        positions: AtomicU64::new(save_data.resume.positions),
        games: AtomicU64::new(save_data.resume.games),
        errored: AtomicBool::new(false),
        stop: AtomicBool::new(false),
    });

    let stats_for_ctrlc = shared_stats.clone();

    println!("Starting datagen");
    ctrlc::set_handler(move || {
        stats_for_ctrlc.stop.store(true, Ordering::SeqCst);
        println!("Stopping...");
    })
    .expect("Error setting Ctrl-C handler");

    let output_folder = &args.output_folder;
    fs::create_dir_all(output_folder).expect("Failed to create output folder");
    // It is truncated after writing, so opening in append mode is better to not truncate if it is never written
    let mut config_file = File::options()
        .write(true)
        .create(true)
        .open(format!("{output_folder}/datagen-config.json"))
        .expect("Failed to open config file (datagen-config.json) for writing");

    let initial_values = InitialDatagenStats {
        intial_positions: save_data.resume.positions,
        intial_games: save_data.resume.games,
        time_from_previous_sessions: save_data.resume.time,
        start_time: Instant::now(),
    };

    thread::scope(|s| {
        let result = {
            let shared_stats = shared_stats.clone();
            // Safety: config_file will not be used outside this closure if it unwinds, shared_stats.errored will be set to gurantee this
            let mut config_file = AssertUnwindSafe(&mut config_file);
            let initial_values = &initial_values;
            // Safety: The closure is not going to make any modifications to save_data that would violate invariants
            // and I am not going to use the value of save_data if unwinding did occur
            let mut save_data = AssertUnwindSafe(&mut save_data);
            catch_unwind(move || {
                for threadnum in 0..args.threads {
                    let shared_stats = shared_stats.clone();
                    s.spawn(move || {
                        let result = catch_unwind(|| {
                            run_datagen_thread(threadnum, &shared_stats, args);
                        });

                        if result.is_err() {
                            shared_stats.errored.store(true, Ordering::SeqCst);
                            shared_stats.stop.store(true, Ordering::SeqCst);
                        }
                    });
                }

                save_data.resume.max_threads_used = save_data.resume.max_threads_used.max(args.threads);

                let mut last_print = Instant::now();
                loop {
                    sleep(Duration::from_secs(10));

                    if shared_stats.stop.load(Ordering::Relaxed) {
                        break;
                    }

                    if last_print.elapsed() >= Duration::from_secs(60) {
                        last_print = Instant::now();

                        // Deref save_data and config_file to remove AssertUnwindSafe wrapper
                        print_stats_and_save_config(&shared_stats, *save_data, *config_file, args, initial_values);
                    }
                }
            })
        };

        // The config_file handle is passed to catch_unwind, so it must not be used after this if unwinding occurred (result.is_err())
        if result.is_err() {
            shared_stats.errored.store(true, Ordering::SeqCst);
            shared_stats.stop.store(true, Ordering::SeqCst);
        }
    });

    if !shared_stats.errored.load(Ordering::Acquire) {
        println!("Final results:");
        let target_met =
            print_stats_and_save_config(&shared_stats, &mut save_data, &mut config_file, args, &initial_values);

        if target_met && args.merge_results {
            let output_folder = &args.output_folder;
            let combined_file_path = format!("{output_folder}/combined.vf");
            let combined_file = File::options().append(true).create(true).open(&combined_file_path);
            let mut combined_file =
                BufWriter::new(combined_file.expect(&format!("Failed to open file {combined_file_path}")));

            for threadnum in 0..save_data.resume.max_threads_used {
                println!("Merging data from thread {threadnum}");
                let thread_data_path = format!("{output_folder}/thread_{threadnum}.vf");
                let thread_data = File::open(&thread_data_path);
                let mut thread_data =
                    BufReader::new(thread_data.expect(&format!("Failed to open file {thread_data_path}")));

                io::copy(&mut thread_data, &mut combined_file)
                    .expect("Error while copying from the thread data file to the combined file");
                combined_file.flush().expect(
                    "Error while flushing  all changes to combined file after copying from the thread data file",
                );

                drop(thread_data);
                fs::remove_file(thread_data_path)
                    .expect("Error while deleting the thread data file after successfully copying");
            }
        }
    }
}

fn run_datagen_thread(threadnum: u16, sync: &Arc<Sync>, args: &DatagenArgs) {
    let output_folder = &args.output_folder;
    let outfile_path = format!("{output_folder}/thread_{threadnum}.vf");
    let outfile = File::options().append(true).create(true).open(&outfile_path);
    let mut outfile = BufWriter::new(outfile.expect(&format!("Failed to open file {outfile_path}")));

    let mut local_games = 0;
    let mut local_games_for_flush = 0;
    let mut local_positions = 0;

    let mut e1_tt = TranspositionTable::new_with_size_mib(args.tt_size_mib).unwrap();
    let mut e2_tt = TranspositionTable::new_with_size_mib(args.tt_size_mib).unwrap();

    let mut repetitions = RepetitionTracker::new();

    let time_control = None;
    let search_control = Some(UciSearchControl::nodes(args.nodes));

    let (_, stop_rx) = mpsc::channel::<()>();

    let mut viri_board = ViriBoard::new();
    viri_board.set_from_fen(&STARTING_FEN, false).unwrap();
    let mut game = Game::new(&viri_board);

    let mut game_moves_vec = Vec::new();
    let mut last_starting_position = None;

    'runGames: loop {
        if sync.stop.load(Ordering::Relaxed) {
            break;
        }

        let fen = STARTING_FEN;

        repetitions.clear();
        // Mucking around with game_moves_vec to avoid reallocating the moves vec and reparsing the FEN for the viri board
        if last_starting_position.is_some_and(|f| f == fen) {
            game.moves = game_moves_vec;
        } else {
            viri_board = ViriBoard::new();
            viri_board.set_from_fen(fen, false).unwrap();
            game = Game::new(&viri_board);
            game.moves = game_moves_vec;
            last_starting_position = Some(fen);
        }
        game.moves.clear();

        let mut fc_board = Board::from_fen(fen, Some(&mut repetitions)).unwrap();

        for _ in 0..args.random_moves_per_side * 2 {
            let mut chosen_move = None;

            let mut moves = ArrayVec::new();
            fc_board.generate_pseudo_legal_moves_without_history(&mut moves);
            let mut rand = rand::rng();
            while moves.len() > 0 {
                let i = rand.random_range(0..moves.len());

                let mov = moves[i].m;

                let mut new_board = fc_board.clone();
                let (legal, _) = new_board.test_legality_and_maybe_make_move(mov, &mut repetitions, None, None);
                if legal {
                    chosen_move = Some(mov);
                    fc_board = new_board;

                    break;
                } else {
                    repetitions.unmake_move(new_board.hash);
                    moves.remove(i);
                }
            }

            if chosen_move.is_none() {
                game_moves_vec = Vec::new();
                mem::swap(&mut game.moves, &mut game_moves_vec);
                // No legal moves - just start a new game
                continue 'runGames;
            }

            game.add_move(chosen_move.unwrap().to_viri_move(), NO_EVAL);
        }

        let current_outcome = check_for_end_of_game(&fc_board, &mut repetitions);
        if current_outcome != GameOutcome::Ongoing {
            game_moves_vec = Vec::new();
            mem::swap(&mut game.moves, &mut game_moves_vec);
            // If the game is over right after the initial random moves, just start a new game.
            continue 'runGames;
        }

        let mut e1_histories;
        let mut e2_histories;

        {
            let tt = if fc_board.white_to_move { &mut e1_tt } else { &mut e2_tt };

            tt.clear();

            let mut histories = ThreadHistoryTables::new();

            let (results, stats) = search_multithreaded(
                1,
                &tt,
                slice::from_mut(&mut histories),
                &stop_rx,
                1,
                RequiredUciOptions::default(),
                0,
                repetitions.clone(),
                PrintMode::None,
                fc_board.clone(),
                &time_control,
                &search_control,
                |_| {},
                false,
                0,
            );

            if results.score.abs() > args.maximum_opening_imbalance {
                game_moves_vec = Vec::new();
                mem::swap(&mut game.moves, &mut game_moves_vec);
                continue;
            }

            // We have decided to continue this game, finish resetting the remembered state
            if fc_board.white_to_move {
                e1_histories = histories;
                e2_histories = ThreadHistoryTables::new();
                e2_tt.clear();
            } else {
                e1_histories = ThreadHistoryTables::new();
                e2_histories = histories;
                e1_tt.clear();
            }

            make_and_record_move(&results, &stats, &mut fc_board, &mut repetitions, &mut game);
        }

        let outcome = loop {
            let current_outcome = check_for_end_of_game(&fc_board, &mut repetitions);
            if current_outcome != GameOutcome::Ongoing {
                break current_outcome;
            }

            let (tt, histories) = if fc_board.white_to_move {
                (&e1_tt, &mut e1_histories)
            } else {
                (&e2_tt, &mut e2_histories)
            };

            let (results, stats) = search_multithreaded(
                1,
                &tt,
                slice::from_mut(histories),
                &stop_rx,
                1,
                RequiredUciOptions::default(),
                0,
                repetitions.clone(),
                PrintMode::None,
                fc_board.clone(),
                &time_control,
                &search_control,
                |_| {},
                false,
                0,
            );

            // Capture, promo, castle, and pawn moves are irreversible. Prevent repetitions from filling up.
            if results.best_move.flags() != 0
                || fc_board.get_piece_64(results.best_move.from() as usize) & PIECE_MASK == PIECE_PAWN
            {
                repetitions.clear();
            }

            make_and_record_move(&results, &stats, &mut fc_board, &mut repetitions, &mut game);
        };

        game.set_outcome(outcome);
        let result = game.serialise_into(&mut outfile);
        if let Err(e) = result {
            println!("Stopping datagen early. Got an error while writing a game to a file: {e}");
            sync.errored.store(true, Ordering::Release);
            sync.stop.store(true, Ordering::Release);
            break;
        }

        local_games += 1;
        local_games_for_flush += 1;
        local_positions += game.len() as u64 + 1;

        if local_games >= 10 {
            sync.games.fetch_add(local_games, Ordering::Relaxed);
            sync.positions.fetch_add(local_positions, Ordering::Relaxed);

            local_games = 0;
            local_positions = 0;
        }

        if local_games_for_flush >= 1000 {
            local_games_for_flush = 0;

            if let Err(e) = outfile.flush() {
                println!("Stopping datagen early. Got an error while periodically flushing generated games to file: {e}");
                sync.errored.store(true, Ordering::Release);
                sync.stop.store(true, Ordering::Release);
                break;
            }
        }

        game_moves_vec = Vec::new();
        mem::swap(&mut game.moves, &mut game_moves_vec);
    }

    sync.games.fetch_add(local_games, Ordering::Relaxed);
    sync.positions.fetch_add(local_positions, Ordering::Relaxed);

    if let Err(e) = outfile.flush() {
        sync.errored.store(true, Ordering::Release);
        println!("Error while writing final data to file {outfile_path}: {e}");
    }
}

fn check_for_end_of_game(board: &Board, repetitions: &mut RepetitionTracker) -> GameOutcome {
    let check_or_stalemate = board.is_checkmate_or_stalemate(repetitions);
    if check_or_stalemate != GameOutcome::Ongoing {
        return check_or_stalemate;
    }

    if board.halfmove_clock >= 100 {
        return GameOutcome::Draw(DrawType::FiftyMoves);
    } else if repetitions.position_has_repeated_times(&board, 3) {
        return GameOutcome::Draw(DrawType::Repetition);
    } else if board.is_insufficient_material() {
        return GameOutcome::Draw(DrawType::InsufficientMaterial);
    }

    return GameOutcome::Ongoing;
}

fn make_and_record_move(
    results: &SearchResult,
    stats: &SearchStats,
    board: &mut Board,
    repetitions: &mut RepetitionTracker,
    game: &mut Game,
) {
    let score = if results.score >= MATE_THRESHOLD {
        i16::MAX
    } else if results.score <= -MATE_THRESHOLD {
        -i16::MAX
    } else {
        results.score
    };

    let white_relative_score = if board.white_to_move { score } else { -score };

    // Positions with only 1 legal move are only searched to depth 1, so the score is bad (but shouldn't produce a wrong mate score)
    game.add_move(
        results.best_move.to_viri_move(),
        if stats.depth == 1 && white_relative_score.abs() != i16::MAX {
            NO_EVAL
        } else {
            white_relative_score
        },
    );

    board.make_move(results.best_move, repetitions, None, None);
}

// Returns whether a target has been hit
fn print_stats_and_save_config(
    shared_stats: &Sync,
    save_data: &mut SavedData,
    config_file: &mut File,
    args: &DatagenArgs,
    initial_values: &InitialDatagenStats,
) -> bool {
    let positions = shared_stats.positions.load(Ordering::Relaxed);
    let games = shared_stats.games.load(Ordering::Relaxed);

    let positions_this_session = positions - initial_values.intial_positions;
    let games_this_session = games - initial_values.intial_games;
    let time_this_session = initial_values.start_time.elapsed();

    let pos_per_sec = positions_this_session as f64 / time_this_session.as_secs_f64();
    let games_per_sec = games_this_session as f64 / time_this_session.as_secs_f64();

    let pos_est_time_left = args
        .target_positions
        .map(|tp| tp.saturating_sub(positions) as f64 / pos_per_sec);
    let games_est_time_left = args
        .target_games
        .map(|tp| tp.saturating_sub(games) as f64 / pos_per_sec);
    let est_time_left = if let Some(pos_est_time_left) = pos_est_time_left
        && let Some(games_est_time_left) = games_est_time_left
    {
        Some(pos_est_time_left.min(games_est_time_left))
    } else {
        pos_est_time_left.or(games_est_time_left)
    };
    let est_time_left_str = est_time_left.map_or(String::new(), |e| {
        let time = {
            let total_seconds = e as u32;
            let hours = total_seconds / 3600;
            let minutes = (total_seconds % 3600) / 60;
            let seconds = total_seconds % 60;
            let without_hours: String = format!("{minutes:02}m {seconds:02}s");
            if hours > 0 {
                format!("{hours}h {without_hours}")
            } else {
                without_hours
            }
        };
        format!(", Estimated time left: {time}")
    });

    let mut positions_fmt = Buffer::new();
    positions_fmt.write_formatted(&positions, &Locale::en);

    let pos_per_sec_int = pos_per_sec as i32;
    let mut pos_per_sec_fmt = Buffer::new();
    pos_per_sec_fmt.write_formatted(&pos_per_sec_int, &Locale::en);

    let mut games_fmt = Buffer::new();
    games_fmt.write_formatted(&games, &Locale::en);

    println!(
        "{positions_fmt} total positions generated ({pos_per_sec_fmt}/s), {games_fmt} games generated ({games_per_sec:.2}/s){est_time_left_str}"
    );

    save_data.resume.positions = positions;
    save_data.resume.games = games;
    save_data.resume.time = time_this_session.saturating_add(initial_values.time_from_previous_sessions);

    let serialzed_save_data = serde_json::to_string_pretty(save_data);
    if let Err(e) = &serialzed_save_data {
        shared_stats.errored.store(true, Ordering::Release);
        shared_stats.stop.store(true, Ordering::Release);
        panic!("Failed to serialize save data to json: {e}");
    }
    if let Err(e) = config_file.seek(SeekFrom::Start(0)) {
        shared_stats.errored.store(true, Ordering::Release);
        shared_stats.stop.store(true, Ordering::Release);
        panic!("Failed to seek to beginning of config file (datagen-config.json): {e}");
    }
    let serialzed_save_data = serialzed_save_data.unwrap();
    let serialized_data_bytes = serialzed_save_data.as_bytes();
    if let Err(e) = config_file.write_all(&serialized_data_bytes) {
        shared_stats.errored.store(true, Ordering::Release);
        shared_stats.stop.store(true, Ordering::Release);
        panic!("Failed to write save data to config file (datagen-config.json): {e}");
    }
    if let Err(e) = config_file.set_len(
        serialized_data_bytes
            .len()
            .try_into()
            .expect("Wrote more bytes than can be stored in a u64, the max the set_len API allows"),
    ) {
        shared_stats.errored.store(true, Ordering::Release);
        shared_stats.stop.store(true, Ordering::Release);
        panic!("Failed to truncate config file (datagen-config.json) after writing: {e}");
    }

    if args
        .target_positions
        .is_some_and(|target_positions| target_positions <= positions)
        || args.target_games.is_some_and(|target_games| target_games <= games)
    {
        shared_stats.stop.store(true, Ordering::Release);
        true
    } else {
        false
    }
}

impl Move {
    fn to_viri_move(self) -> ViriMove {
        let from = Square::new(self.from()).unwrap();
        let to = Square::new(self.to()).unwrap();
        let flags = self.flags();
        if flags == 0 || flags == MOVE_FLAG_CAPTURE || flags == MOVE_DOUBLE_PAWN {
            ViriMove::new(from, to)
        } else if self.is_promo() {
            // viri has pawn = 0, knight = 1, ... queen = 4. So 1 less than foxchess
            let piece_type = ((flags as u8) & 3) + 1;
            ViriMove::new_with_promo(from, to, PieceType::new(piece_type).unwrap())
        } else if flags == MOVE_EP_CAPTURE {
            ViriMove::new_with_flags(from, to, MoveFlags::EnPassant)
        } else if flags == MOVE_KING_CASTLE || flags == MOVE_QUEEN_CASTLE {
            // https://crates.io/crates/viriformat says king takes rook
            let file = if flags == MOVE_QUEEN_CASTLE { 0 } else { 7 };
            let rook = (self.to() & !7) + file;
            ViriMove::new_with_flags(from, Square::new(rook).unwrap(), MoveFlags::Castle)
        } else {
            panic!(
                "Unable to convert move to viri move: {} ({})",
                self.pretty_print(None),
                self.data
            )
        }
    }
}

impl Board {
    /// Will make no net modifications to repetitions.
    fn is_checkmate_or_stalemate(&self, repetitions: &mut RepetitionTracker) -> GameOutcome {
        let mut move_generator = StagedMoveGenerator::new();
        let in_check = self.is_in_check(false);
        if in_check {
            move_generator.generate_moves_check_evasion(self, None, None, None, None, None);
        } else {
            move_generator.generate_moves_pseudo_legal(self);
        }

        while let Some(mov) = move_generator.get_next_move_unordered(self) {
            let mut new_board = self.clone();
            let (legal, move_made) = new_board.test_legality_and_maybe_make_move(mov, repetitions, None, None);
            if legal {
                repetitions.unmake_move(new_board.hash);
                return GameOutcome::Ongoing;
            } else if move_made {
                repetitions.unmake_move(new_board.hash);
            }
        }

        if in_check {
            if self.white_to_move {
                GameOutcome::BlackWin(WinType::Mate)
            } else {
                GameOutcome::WhiteWin(WinType::Mate)
            }
        } else {
            GameOutcome::Draw(DrawType::Stalemate)
        }
    }
}
