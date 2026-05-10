use std::{
    cell::LazyCell,
    collections::HashMap,
    fs::File,
    io::{self, BufRead, BufReader, Read, Seek, SeekFrom, Write},
    path::Path,
    time::{Duration, Instant},
};

use rand::{RngExt, SeedableRng, rngs::StdRng};
use regex::Regex;

use crate::{
    STARTING_FEN,
    board::{
        Board, PIECE_BISHOP, PIECE_MASK, PIECE_PAWN, PIECE_QUEEN, PIECE_ROOK, file_8x8, piece_to_letter, rank_8x8,
    },
    cli::TuningArgs,
    moves::{MOVE_KING_CASTLE, MOVE_QUEEN_CASTLE, Move},
    repetition_tracker::RepetitionTracker,
    staged_move_generator::StagedMoveGenerator,
};

const NORMAL_MOVE_REGEX_STR: &str = r"^(?P<movPiece>[BKNRQ])?(?P<srcFile>[a-h])?(?P<srcRank>[1-8])?x?(?P<dest>[a-h][1-8])(?P<promo>=[BNRQ])?(?P<check>\+|#)?$";

thread_local! {
    static TAG_REGEX: LazyCell<Regex> = LazyCell::new(|| Regex::new(r#"\[(\w*) "([^"]*)"\]"#).unwrap());
    static MOVE_NUMBER_REGEX: LazyCell<Regex> = LazyCell::new(|| Regex::new(r"^\d+\.{1,3}$").unwrap());
    static FULL_MOVE_REGEX: LazyCell<Regex> = LazyCell::new(|| Regex::new(format!(r"^(O-O(\+|#)?|O-O-O(\+|#)?|{NORMAL_MOVE_REGEX_STR})$").as_str()).unwrap());
    static NORMAL_MOVE_REGEX: LazyCell<Regex> = LazyCell::new(|| Regex::new(NORMAL_MOVE_REGEX_STR).unwrap());
}

pub struct Pgn {
    tags: HashMap<String, String>,
    moves: Vec<PgnMove>,
    board: Board,
    fens: Vec<String>,
    starting_move_num: u16,
    started_as_white: bool,
}

pub struct PgnMove {
    pub mov: String,
    pub comment: Option<String>,
}

impl<'a> Pgn {
    pub fn parse_pgn(reader: &mut BufReader<File>) -> Result<Self, String> {
        let mut finished_tags = false;
        let mut tags = HashMap::new();
        let mut moves = Vec::new();

        loop {
            let mut buf = vec![0xFF];
            reader.read(&mut buf[0..1]).is_at_least_one()?;
            if buf[0] == b'\r' {
                buf.push(0xFF);
                reader.read(&mut buf[1..2]).is_at_least_one()?;

                if buf[1] != b'\n' {
                    return Err(format!(
                        "Found CR not followed by LF at {}",
                        try_get_stream_position(reader)
                    ));
                }
            }

            if buf[0] == b'[' {
                if finished_tags {
                    return Err(format!("Found unexpected tag at {}", try_get_stream_position(reader)));
                }

                let mut str = String::from_utf8(buf).unwrap();
                reader.read_line(&mut str).is_at_least_one()?;

                TAG_REGEX.with(|tr| {
                    if let Some(captures) = tr.captures(&str) {
                        tags.insert(captures[1].to_string(), captures[2].to_string());
                        Ok(())
                    } else {
                        Err(format!(
                            "Tag does not match expected format at {}",
                            try_get_stream_position(reader)
                        ))
                    }
                })?;
            } else {
                finished_tags = true;
                if tags.is_empty() {
                    return Err(format!(
                        "Tried to start a new game at {}, but no tags were found",
                        try_get_stream_position(reader)
                    ));
                }

                if buf[0] == b'\r' {
                    buf.pop();
                    buf.pop();
                } else if buf[0] == b'\n' {
                    buf.pop();
                }

                // Need to change to also detect end of game
                // Read move number ex 1. or 18...
                read_to_space_or_newline(reader, &mut buf, true)?;
                let str = str::from_utf8(&buf).unwrap().trim_ascii();
                let mov;

                // fastchess does not put 1... before black's move, but pgn-extract processed PGNs do put that.
                if !FULL_MOVE_REGEX.with(|mr| mr.is_match(str)) {
                    if !MOVE_NUMBER_REGEX.with(|mnr| mnr.is_match(str)) {
                        if str.starts_with("1-0") && str.starts_with("1/2-1/2") && str.starts_with("0-1") {
                            return Err(format!(
                                "Expected a move number or game result but found {str} at {}",
                                try_get_stream_position(reader)
                            ));
                        } else {
                            break;
                        }
                    }

                    buf.clear();

                    // Read move
                    read_to_space_or_newline(reader, &mut buf, false)?;

                    while !buf.is_empty() && char::from(buf[buf.len() - 1]).is_ascii_whitespace() {
                        buf.pop();
                    }

                    mov = String::from_utf8(buf).unwrap();
                    if !FULL_MOVE_REGEX.with(|mr| mr.is_match(&mov)) {
                        return Err(format!(
                            "Expected a move but found {mov} at {}",
                            try_get_stream_position(reader)
                        ));
                    }
                } else {
                    while !buf.is_empty() && char::from(buf[buf.len() - 1]).is_ascii_whitespace() {
                        buf.pop();
                    }

                    mov = String::from_utf8(buf).unwrap();
                }

                let mut comment_buf = Vec::with_capacity(1);
                loop {
                    comment_buf.push(0xFF);
                    let len = comment_buf.len();
                    reader.read(&mut comment_buf[len - 1..len]).is_at_least_one()?;
                    // The value that was read is not actually part of the comment text
                    let delimiter_start = comment_buf.pop().unwrap();

                    if delimiter_start == b';' {
                        reader.read_until(b'\n', &mut comment_buf).is_at_least_one()?;
                        if comment_buf.len() > 1 && comment_buf[comment_buf.len() - 2] == b'\r' {
                            // pop an extra time to clear the full CRLF
                            comment_buf.pop();
                        }
                    } else if delimiter_start == b'{' {
                        reader.read_until(b'}', &mut comment_buf).is_at_least_one()?;
                    } else if delimiter_start == b'(' {
                        reader.read_until(b')', &mut comment_buf).is_at_least_one()?;
                    } else {
                        // Something was read that wasn't part of a comment, put it back
                        reader
                            .seek_relative(-1)
                            .map_err(|err| format!("Failed to seek backwards after reading comments: {err}"))?;
                        break;
                    }

                    // Remove the ending delimiter
                    comment_buf.pop();
                    // Read a trailing whitespace character
                    comment_buf.push(0xFF);
                    let len = comment_buf.len();
                    reader.read(&mut comment_buf[len - 1..len]).is_at_least_one()?;
                    let whitespace = comment_buf.pop().unwrap();
                    if whitespace == b'\r' {
                        comment_buf.push(0xFF);
                        reader.read(&mut comment_buf[len - 1..len]).is_at_least_one()?;
                        let whitespace = comment_buf.pop().unwrap();

                        if whitespace != b'\n' {
                            return Err(format!(
                                "Found CR not followed by LF at {}",
                                try_get_stream_position(reader)
                            ));
                        }
                    }
                }

                let comment = if comment_buf.is_empty() {
                    None
                } else {
                    let comment_str = String::from_utf8(comment_buf).unwrap();
                    let comment_str = comment_str.replace("\r\n", " ");
                    let comment_str = comment_str.replace('\n', " ");

                    Some(comment_str)
                };
                moves.push(PgnMove { mov, comment });
            }
        }

        // Multi-game PGN files are expected to have a blank line between the games
        reader.skip_until(b'\n').unwrap();

        if moves.is_empty() {
            Err("Found no moves".to_string())
        } else if let Some(plys_str) = tags.get("PlyCount")
            && let Ok(plys) = plys_str.parse::<u32>()
            && moves.len() != plys as usize
        {
            Err(format!(
                "Expected to find {plys} moves but found {} moves in game that is believed to end at position {}",
                moves.len(),
                try_get_stream_position(reader)
            ))
        } else {
            let fen = tags.get("FEN").map(|v| v.as_str()).unwrap_or(STARTING_FEN);
            let board = Board::from_fen(fen, None).unwrap();
            let starting_move_num = board.fullmove_counter;
            let started_as_white = board.white_to_move;
            Ok(Pgn {
                tags,
                moves,
                board,
                fens: Vec::new(),
                starting_move_num,
                started_as_white,
            })
        }
    }

    pub fn print(&self) -> Result<String, String> {
        let mut result = String::with_capacity(self.tags.len() * 20 + self.moves.len() * 25);

        // Note: tag order will not match the original order
        for tag in &self.tags {
            result.push('[');
            result.push_str(tag.0);
            result.push_str(" \"");
            result.push_str(tag.1);
            result.push_str("\"]\n");
        }

        result.push('\n');

        for (i, mov) in self.moves.iter().enumerate() {
            if i.is_multiple_of(2) {
                result.push_str(&((i as u16 / 2) + self.starting_move_num).to_string());
                result.push_str(". ");
            }

            result.push_str(&mov.mov);

            // Note: Comments with comment delimiters inside them are not handled properly
            // ex. { [%clk 0:00:26] } ( 115... Ka3 116. Rc4 Ka2 117. Ra4# { [%eval #2,4] } )
            if let Some(comment) = &mov.comment {
                if comment.contains('}') {
                    return Err(
                        "A comment contains a '}' character, which is not properly handled at this time.".to_string(),
                    );
                }

                result.push_str(" {");
                result.push_str(comment);
                result.push('}');
                if i.is_multiple_of(2) {
                    result.push(' ');
                }
            }

            if !i.is_multiple_of(2) {
                result.push('\n');
            }
        }

        if let Some(game_result) = self.tags.get("Result") {
            result.push_str(game_result);
            result.push('\n');
        }

        Ok(result)
    }

    /// Assumes that the SAN includes an 'x' when capturing a piece
    pub fn is_move_capture(&self, idx: usize) -> bool {
        self.moves[idx].mov.contains('x')
    }

    pub fn is_move_promotion(&self, idx: usize) -> bool {
        self.moves[idx].mov.contains('=')
    }

    /// First position is before the first move. For other positions, assumes that the SAN includes a '+' or '#' when appropriate.
    pub fn is_position_in_check(&self, idx: usize) -> bool {
        if idx != 0 {
            self.moves[idx - 1].mov.ends_with('+') || self.moves[idx - 1].mov.ends_with('#')
        } else {
            let board = Board::from_fen(self.get_starting_fen(), None).unwrap();
            board.is_in_check(false)
        }
    }

    fn get_starting_fen(&'a self) -> &'a str {
        self.tags.get("FEN").map(|v| v.as_str()).unwrap_or(STARTING_FEN)
    }

    pub fn get_fen_for_position(&'a mut self, idx: usize) -> &'a str {
        if idx == 0 {
            self.get_starting_fen()
        } else {
            let mut repetitions = RepetitionTracker::default();

            while idx > self.fens.len() {
                let move_index = self.fens.len();

                if let Some(move_parts) = NORMAL_MOVE_REGEX.with(|mr| mr.captures(&self.moves[move_index].mov)) {
                    let dest_chars: Vec<char> = move_parts["dest"].chars().collect();
                    let dest_sq =
                        ((dest_chars[0] as u8 - b'a') + (8 * (dest_chars[1].to_digit(10).unwrap() as u8 - 1))) as u16;

                    let moving_piece = move_parts
                        .name("movPiece")
                        .map_or("P", |m| m.as_str())
                        .chars()
                        .next()
                        .unwrap();

                    let src_file = move_parts.name("srcFile").map(|f| f.as_str().as_bytes()[0] - b'a');
                    let src_rank = move_parts.name("srcRank").map(|f| f.as_str().as_bytes()[0] - b'0');
                    let promo_piece = move_parts.name("promo").map(|f| f.as_str().chars().nth(1).unwrap());

                    let mut move_gen = StagedMoveGenerator::new();
                    move_gen.generate_moves_pseudo_legal(&self.board);

                    let mut move_matched = false;
                    let board_copy = self.board.clone();
                    while let Some(mov) = move_gen.get_next_move_unordered(&self.board) {
                        if mov.to() != dest_sq {
                            continue;
                        }

                        let moving_piece_type = self.board.get_piece_64(mov.from() as usize) & PIECE_MASK;
                        if piece_to_letter(moving_piece_type) != moving_piece {
                            continue;
                        }

                        if src_file.is_some_and(|sf| sf != file_8x8(mov.from() as u8))
                            || src_rank.is_some_and(|sr| sr != rank_8x8(mov.from() as u8))
                            || promo_piece.is_some_and(|promo_piece| {
                                promo_piece != piece_to_letter(((mov.flags() as u8) & 3) + 2)
                            })
                        {
                            continue;
                        }

                        let (legal, move_made) = self.board.test_legality_and_maybe_make_move(mov, &mut repetitions);
                        if !legal {
                            // If two pieces can pseudolegally move to a square but one of them is pinned,
                            // then the disambiguating rank/file may not be printed in the PGN so we need to backtrack
                            if move_made {
                                self.board = board_copy.clone();
                            }

                            continue;
                        }

                        move_matched = true;
                        break;
                    }

                    if !move_matched {
                        panic!(
                            "Did not find any legal move matching {} for position {}",
                            self.moves[move_index].mov,
                            self.fens.last().map_or_else(|| self.get_starting_fen(), |f| f.as_str()),
                        );
                    }
                } else {
                    let mov = if self.moves[move_index].mov.starts_with("O-O-O") {
                        self.find_castle_move(MOVE_QUEEN_CASTLE)
                    } else if self.moves[move_index].mov.starts_with("O-O") {
                        self.find_castle_move(MOVE_KING_CASTLE)
                    } else {
                        panic!("Move {} did not match expected move format", self.moves[move_index].mov);
                    };

                    let (legal, _) = self.board.test_legality_and_maybe_make_move(mov, &mut repetitions);
                    if !legal {
                        panic!(
                            "Described move {} matched generated move {}, but it is illegal for position {}",
                            self.moves[move_index].mov,
                            mov.pretty_print(None),
                            self.fens.last().map_or_else(|| self.get_starting_fen(), |f| f.as_str()),
                        );
                    }
                }

                repetitions.unmake_move(self.board.hash);
                self.fens.push(self.board.to_fen());
            }

            &self.fens[idx - 1]
        }
    }

    fn find_castle_move(&mut self, flag: u16) -> Move {
        let mut move_gen = StagedMoveGenerator::new();
        move_gen.generate_moves_pseudo_legal(&self.board);

        while let Some(mov) = move_gen.get_next_move_unordered(&self.board) {
            if mov.flags() == flag {
                return mov;
            }
        }

        panic!(
            "Did not find any move matching {} for position {}",
            if flag == MOVE_QUEEN_CASTLE {
                "Queenside castle"
            } else {
                "Kingside castle"
            },
            self.board.to_fen()
        );
    }

    /// Returns Err if the fen has not been stored already
    pub fn get_stored_fen(&'a self, idx: usize) -> Result<&'a str, ()> {
        if idx == 0 {
            Ok(self.get_starting_fen())
        } else if idx - 1 < self.fens.len() {
            Ok(&self.fens[idx - 1])
        } else {
            Err(())
        }
    }

    pub fn get_moves(&'a self) -> &'a Vec<PgnMove> {
        &self.moves
    }

    pub fn get_tags(&'a self) -> &'a HashMap<String, String> {
        &self.tags
    }
}

/// Made to match the format output by pgn-extract's -Wepd. Includes my custom extension that adds an extra comment with the next move's comment.
/// Will only match pgn-extract output exactly if --nofauxep is passed to pgn-extract.
pub fn print_epds_for_pgn(in_filename: &Path, include_all_terminations: bool) {
    let mut reader = BufReader::new(File::open(in_filename).unwrap());
    let file_len = reader.seek(SeekFrom::End(0)).unwrap();
    reader.seek(SeekFrom::Start(0)).unwrap();

    let mut stdout = io::stdout().lock();
    // Add a buffer of 4 in case there is an empty line or two at the end of the file
    while reader.stream_position().unwrap() + 4 < file_len {
        let pgn = Pgn::parse_pgn(&mut reader);
        if let Ok(mut pgn) = pgn {
            let tags = pgn.get_tags();

            if !include_all_terminations && (!tags.contains_key("Termination") || tags["Termination"] != "normal") {
                continue;
            }

            let epd_c0_c1 = format!(
                " c0 {}-{} {} {} {}; c1 {};",
                tags["White"], tags["Black"], tags["Event"], tags["Site"], tags["Date"], tags["Result"]
            );

            for i in 0..=pgn.get_moves().len() {
                let fen = pgn.get_fen_for_position(i);
                // Split the move numbers off of the FEN
                let fen_part = fen.rsplitn(3, ' ').nth(2).unwrap();

                write!(stdout, "{fen_part}").unwrap();
                write!(stdout, "{}", epd_c0_c1).unwrap();

                if i != 0
                    && let Some(comment) = pgn.get_moves()[i - 1].comment.as_ref()
                {
                    write!(stdout, " c2 {comment};").unwrap();
                }

                writeln!(stdout).unwrap();
            }

            writeln!(stdout).unwrap();
        } else {
            let err = pgn.err().unwrap();
            eprintln!("{}", err);
        }
    }
}

/// Selects quiet positions for tuning and prints to stdout.
pub fn print_tuning_positions(args: &TuningArgs) {
    let normal_term_games_count;
    let positions_per_game;
    let extra_positions;
    let mut extra_positions_left;
    let mut rand = StdRng::seed_from_u64(0x88d885d4bb51ffc2);
    let score_regex = Regex::new(r"^[+-]?\d+\.\d\d").unwrap();
    let mate_regex = Regex::new(r"^[+-]?M\d+").unwrap();
    let single_depth_search_regex = Regex::new(r"^([+-]?M\d+|[+-]?\d+\.\d\d)\/1 ").unwrap();
    let start_time = Instant::now();
    let actual_total_games_count = BufReader::new(File::open(&args.filepath).unwrap())
        .lines()
        .filter(|l| l.as_ref().unwrap().starts_with("[Termination"))
        .count();

    if !args.sampling.no_random_sampling {
        normal_term_games_count = BufReader::new(File::open(&args.filepath).unwrap())
            .lines()
            .filter(|l| l.as_ref().unwrap() == r#"[Termination "normal"]"#)
            .count() as u32;
        let positions_to_use = args.sampling.positions_to_choose;
        positions_per_game = positions_to_use / normal_term_games_count;
        // Find out how many positions we will be short because of integer rounding
        extra_positions = positions_to_use - positions_per_game * normal_term_games_count;
        extra_positions_left = extra_positions;
    } else {
        normal_term_games_count = 0;
        positions_per_game = 0;
        extra_positions = 0;
        extra_positions_left = 0;
    }

    let mut games_parsed = 0;
    let mut games_parsed_milestone = actual_total_games_count / 20;
    let mut used_games = 0;
    let mut used_positions = 0;
    let mut reader = BufReader::new(File::open(&args.filepath).unwrap());
    let file_len = reader.seek(SeekFrom::End(0)).unwrap();
    reader.seek(SeekFrom::Start(0)).unwrap();

    let mut stdout = io::stdout().lock();
    // Add a buffer of 4 in case there is an empty line or two at the end of the file
    while reader.stream_position().unwrap() + 4 < file_len {
        let pgn = Pgn::parse_pgn(&mut reader);
        if let Ok(mut pgn) = pgn {
            games_parsed += 1;
            if games_parsed > games_parsed_milestone {
                games_parsed_milestone += actual_total_games_count / 20;
                let elapsed = start_time.elapsed();
                if elapsed > Duration::from_secs(10) {
                    let percentage = games_parsed as f32 / actual_total_games_count as f32;
                    let estimated_total_time = elapsed.div_f32(percentage);
                    let estimated_time_left = estimated_total_time - elapsed;
                    eprintln!(
                        "Processed {games_parsed} ({:.0}%) games so far. Estimated time left: {estimated_time_left:#?}",
                        percentage * 100.0
                    );
                }
            }

            let tags = pgn.get_tags();

            if !tags.contains_key("Termination") || tags["Termination"] != "normal" || !tags.contains_key("Result") {
                continue;
            }

            let game_result = tags["Result"].clone();

            if !args.include_kbnk_draws && game_result == "1/2-1/2" {
                let moves_count = pgn.get_moves().len();
                // This will advance the board to the 2nd to last position in the game
                pgn.get_fen_for_position(moves_count - 1);

                if pgn.board.side_occupancy[0].count_ones() == 1 || pgn.board.side_occupancy[1].count_ones() == 1 {
                    let white_has_piece = pgn.board.side_occupancy[0].count_ones() > 1;
                    let winning_side = if white_has_piece { 0 } else { 1 };

                    if pgn.board.piece_bitboards[winning_side][PIECE_QUEEN as usize] == 0
                        && pgn.board.piece_bitboards[winning_side][PIECE_ROOK as usize] == 0
                        && pgn.board.piece_bitboards[winning_side][PIECE_PAWN as usize] == 0
                        && pgn.board.piece_bitboards[winning_side][PIECE_BISHOP as usize].count_ones() == 1
                    {
                        continue;
                    }
                }
            }

            used_games += 1;

            let lower_valid_positions_bound = pgn
                .get_moves()
                .iter()
                .position(|m| {
                    (args.include_no_comment_moves || m.comment.is_some())
                        && (args.include_book_moves || m.comment.as_ref().is_none_or(|c| c != "book"))
                })
                .unwrap_or(pgn.get_moves().len());
            let mut upper_valid_positions_bound = pgn
                .get_moves()
                .iter()
                .rposition(|m| {
                    m.comment
                        .as_ref()
                        .is_some_and(|c| !mate_regex.is_match(c) && !single_depth_search_regex.is_match(c))
                })
                .unwrap_or(0);

            if (args.draw_adj_min_ply.is_some() && args.draw_adj_moves.is_some() && args.draw_adj_score.is_some())
                || (args.win_adj_moves.is_some() && args.win_adj_score.is_some())
            {
                let mut win_scores_run: i16 = 0;
                let mut turn_sign = 1;
                let mut draw_scores_run = 0;

                'adj_loop: for (i, m) in pgn.get_moves().iter().enumerate() {
                    if m.comment.is_none() {
                        win_scores_run = 0;
                        draw_scores_run = 0;
                        turn_sign *= -1;
                        continue;
                    }

                    let score = score_regex
                        .find(m.comment.as_ref().unwrap())
                        .map(|s| (s.as_str().parse::<f32>().unwrap() * 100.0) as i16);

                    if let Some(win_adj_moves) = args.win_adj_moves
                        && let Some(win_adj_score) = args.win_adj_score
                    {
                        let mate_sign = if score.is_some() {
                            None
                        } else {
                            mate_regex
                                .find(m.comment.as_ref().unwrap())
                                .map(|m| if m.as_str().starts_with('-') { -1 } else { 1 })
                        };
                        let mut run_adjusted = false;
                        for side_sign in [-1, 1] {
                            if score.is_some_and(|s| s * turn_sign * side_sign >= win_adj_score)
                                || mate_sign.is_some_and(|s| s * turn_sign * side_sign > 0)
                            {
                                run_adjusted = true;
                                if win_scores_run.signum() == side_sign {
                                    win_scores_run += side_sign;
                                } else {
                                    win_scores_run = side_sign;
                                }

                                if win_scores_run.abs() as u16 >= win_adj_moves {
                                    upper_valid_positions_bound = upper_valid_positions_bound.min(i);
                                    break 'adj_loop;
                                }
                            }
                        }

                        if !run_adjusted {
                            win_scores_run = 0;
                        }
                    }

                    if let Some(draw_adj_min_ply) = args.draw_adj_min_ply
                        && let Some(draw_adj_moves) = args.draw_adj_moves
                        && let Some(draw_adj_score) = args.draw_adj_score
                    {
                        // There is a bug where if the ply given in draw_adj_min_ply was part of the opening epd then no draw adjucation is done
                        if i as u16 + (pgn.starting_move_num - 1) * 2 + if pgn.started_as_white { 0 } else { 1 }
                            >= draw_adj_min_ply
                        {
                            if score.is_some_and(|s| s.abs() <= draw_adj_score) {
                                draw_scores_run += 1;
                                if draw_scores_run >= draw_adj_moves {
                                    upper_valid_positions_bound = upper_valid_positions_bound.min(i);
                                    break;
                                }
                            } else {
                                draw_scores_run = 0;
                            }
                        }
                    }

                    turn_sign *= -1;
                }
            }

            let mut positions_to_take =
                if extra_positions_left > 0 && rand.random_ratio(extra_positions, normal_term_games_count) {
                    extra_positions_left -= 1;
                    positions_per_game + 1
                } else {
                    positions_per_game
                };

            let mut possible_positions: Vec<usize> =
                (lower_valid_positions_bound..=upper_valid_positions_bound).collect();

            if possible_positions.is_empty() {
                eprintln!(
                    "Warn: possible_positions is 0 for game before file position {}",
                    try_get_stream_position(&mut reader)
                );
            }

            while !possible_positions.is_empty() && (args.sampling.no_random_sampling || positions_to_take > 0) {
                let position = possible_positions.swap_remove(rand.random_range(0..possible_positions.len()));

                // Look for quiet positions (bestmove not a capture or promotion, not in check) where mate is not seen
                if pgn.is_move_capture(position)
                    || pgn.is_move_promotion(position)
                    || pgn.is_position_in_check(position)
                    || pgn.get_moves()[position]
                        .comment
                        .as_ref()
                        .is_some_and(|c| mate_regex.is_match(c) || single_depth_search_regex.is_match(c))
                {
                    continue;
                }

                positions_to_take -= 1;
                used_positions += 1;

                let fen = pgn.get_fen_for_position(position);
                // Split the move numbers off of the FEN
                let fen_part = fen.rsplitn(3, ' ').nth(2).unwrap();
                write!(stdout, "{fen_part};{game_result}").unwrap();

                if args.print_score
                    && let Some(comment) = &pgn.get_moves()[position].comment
                    && let Some(score) = score_regex.find(comment)
                {
                    write!(stdout, ";{}", score.as_str()).unwrap();
                }

                writeln!(stdout).unwrap();
            }

            if positions_to_take > 0 {
                extra_positions_left += positions_to_take;
            }
        } else {
            let err = pgn.err().unwrap();
            eprintln!("Failed to parse a game: {}", err);
        }
    }

    let elapsed = start_time.elapsed();
    eprintln!(
        "{used_positions} positions selected using {used_games} games. Targetted {positions_per_game} positions per game. Took {elapsed:#?}."
    );
}

pub fn reprint_pgns(in_filename: &Path) {
    let mut reader = BufReader::new(File::open(in_filename).unwrap());

    let mut games = 0;

    let file_len = reader.seek(SeekFrom::End(0)).unwrap();
    reader.seek(SeekFrom::Start(0)).unwrap();

    let mut stdout = io::stdout().lock();
    // Add a buffer of 4 in case there is an empty line or two at the end of the file
    while reader.stream_position().unwrap() + 4 < file_len {
        let pgn = Pgn::parse_pgn(&mut reader);
        if let Ok(pgn) = pgn {
            games += 1;
            writeln!(stdout, "{}", pgn.print().unwrap()).unwrap();
        } else {
            let err = pgn.err().unwrap();
            eprintln!("{}", err);
        }
    }
    eprintln!("Parsed {games} games");
}

fn try_get_stream_position(reader: &mut BufReader<File>) -> String {
    reader
        .stream_position()
        .map_or("Unknown position".to_string(), |v| format!("{v}"))
}

// It would probably be better to fill the reader's buffer and then scan until the next space or newline and just read it all at once
fn read_to_space_or_newline(reader: &mut BufReader<File>, buf: &mut Vec<u8>, allow_eof: bool) -> Result<usize, String> {
    let mut bytes_read = 0;

    loop {
        buf.push(0xFF);
        let len = buf.len();
        let read_result = reader.read(&mut buf[len - 1..len]);

        if !allow_eof {
            read_result.is_at_least_one()?;
        } else if let Err(err) = &read_result {
            return Err(format!("Error received: {}", err.kind()));
        } else if let Ok(count_read) = &read_result
            && *count_read == 0
        {
            // if EOF, pop the dummy value off before returning
            buf.pop();
            return Ok(bytes_read);
        }

        bytes_read += 1;

        if buf[buf.len() - 1] == b' ' || buf[buf.len() - 1] == b'\n' {
            return Ok(bytes_read);
        }
    }
}

trait IsAtLeastOne {
    fn is_at_least_one(&self) -> Result<(), String>;
}

impl IsAtLeastOne for Result<usize, std::io::Error> {
    fn is_at_least_one(&self) -> Result<(), String> {
        if let Err(err) = &self {
            Err(format!("Error received: {}", err.kind()))
        } else if self.as_ref().is_ok_and(|v| *v == 0) {
            Err("Unexpected EOF".to_string())
        } else {
            Ok(())
        }
    }
}
