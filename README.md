# Fox chess
:3

A UCI chess engine. Intended for use with a chess GUI such as Cute Chess or Arena or interfacing with a chess server with a program like lichess-bot.

The program is available for challenge some of the time at [lichess](https://lichess.org/@/FoxChessBot). I run it on my own computer so availability is sporadic and is not guranteed.

### Prerequisites for Building
The MSRV is currently 1.88.

To create a PGO optimized build (recommended) you will need the `llvm-profdata` binary which can be installed with:
```
rustup component add llvm-tools-preview
```

## Building
You are recommended to enable your CPU's supported features for the best performance. If you will be running the program on the same computer you are building, you can use
```
RUSTFLAGS=-Ctarget-cpu=native cargo pgo run -- bench
RUSTFLAGS=-Ctarget-cpu=native cargo pgo optimize
```
Or on x86_64-pc-windows-msvc that will be enabled automatically and you can simply run
```
cargo pgo run -- bench
cargo pgo optimize
```
To skip PGO optimization, just run
```
cargo build -r
```

### Note for Ryzen CPUs below 5000 series
These CPUs support but have poor performance for some instructions used for determining sliding piece attacks. Before building you should disable the `use_pext` feature in `Cargo.toml` for the best performance.

### Building for another computer
Based on the supported features of the computer you are building for, you should select one of these options

|Features|Target|
|--------|-------|
|AVX512|x86-64-v4|
|AVX2 and BMI2|x86-64-v3|
|POPCNT|x86-64-v2|

Replace `<target>` in the below command with the target above based on the computer's supported features
```
RUSTFLAGS=-Ctarget-cpu=<target> cargo pgo run -- bench
RUSTFLAGS=-Ctarget-cpu=<target> cargo pgo optimize
```

If you're unsure, you can instead just use (although performance will be impacted)
```
cargo pgo run -- bench
cargo pgo optimize
```

If you want to build a fully portable executable on x86_64-pc-windows-msvc then you will need to explicitly pass `x86-64-v1` for the target

## Credits
Many thanks to the [Chess Programming Wiki](https://www.chessprogramming.org) for explaining the basic and advanced concepts of creating a chess engine

The Engine Programming Discord for interesting discussions and ideas

Other open source chess engines

## Ideas of features to add
- [x] Needs some help in endgames. It lost KQK because it wouldn't move the king out of the center to support the queen. Greater depth helps with this.
  - [ ] Still needs help with harder endgames
- [x] Avoid draws due to insufficient mating material
  - [ ] [Simple heuristics](https://www.chessprogramming.org/Draw_Evaluation) for not quite drawn games
- [ ] [Consider checks during quiesence search](https://www.chessprogramming.org/Quiescence_Search#Checks)
  - [ ] [Specialized check-only move generation](https://www.chessprogramming.org/Move_Generation#Special_Generators)
  - Tested this and it was worse
- [x] [Delta pruning](https://www.chessprogramming.org/Delta_Pruning)
- [x] [Transposition table](https://www.chessprogramming.org/Transposition_Table)
  - [ ] Better replacement scheme. Can pack some data into part of the hash, using the index to recover the rest of the hash.
    - This was added but I still wonder if it could be better
- [x] [Iterative deepening](https://www.chessprogramming.org/Iterative_Deepening)
  - [x] [Aspiration window](https://www.chessprogramming.org/Aspiration_Windows)
- [x] [Null move pruning](https://www.chessprogramming.org/Null_Move_Pruning)
- [ ] [Some kind of better Move Ordering](https://www.chessprogramming.org/Move_Ordering)
  - [x] [Killer heuristic](https://www.chessprogramming.org/Killer_Heuristic)
  - [x] [History heuristic](https://www.chessprogramming.org/History_Heuristic)
  - [ ] Internal Iterative Deepening
- [ ] [Search Selectivity](https://www.chessprogramming.org/Selectivity)
  - [x] Null Move Pruning
    - I think conditions for when this is applied could be improved still
  - [x] [Late move reduction](https://www.chessprogramming.org/Late_Move_Reductions)
  - [ ] [Razoring](https://www.chessprogramming.org/Razoring)
  - [ ] [Futility Pruning](https://www.chessprogramming.org/Futility_Pruning)
  - [ ] [Reverse Futility Pruning](https://www.chessprogramming.org/Reverse_Futility_Pruning)
  - [ ] [Improving](https://www.chessprogramming.org/Improving) modifier for other strategies
- [x] Refine piece square evaluations
  - [x] [Texel Tuning](https://www.chessprogramming.org/Texel%27s_Tuning_Method)
- [ ] Evaluation
  - [ ] [Consider Mobility (and negative mobility for king)](https://www.chessprogramming.org/Mobility)
    - Tested negative mobility for the king and it was worse
  - [ ] Pawn evaluation
    - [ ] [Pawn hash table](https://www.chessprogramming.org/Pawn_Hash_Table)
    - [ ] Isolated pawns
      - Coded but disabled as it tested worse
    - [x] Doubled pawns
    - [ ] [Pawn chains](https://www.chessprogramming.org/Connected_Pawns)
    - [ ] [Passed pawns](https://www.chessprogramming.org/Passed_Pawn)
      - [x] In general
      - [ ] Pawn Race
      - [ ] Rule of the Square
      - [ ] Connected Passed Pawns
- [x] [Use bitboards to improve move generation](https://www.chessprogramming.org/Bitboards)
  - [x] Use pext when available instead of magic numbers for rook and bishop attacks
- [ ] Opening book
- [ ] Endgame tablebase
- [ ] [Contempt factor](https://www.chessprogramming.org/Contempt_Factor)
  - [ ] Judge the opponent's moves based on our PV and how they change the eval like described in [this thread](https://www.talkchess.com/forum/viewtopic.php?p=531133#p531133)
- [x] Redo mate scoring so at mate it returns the full 20000cp and then lower it as it walks up the tree. That way each node in tt will have the proper score.
- [x] Redo something about evaluation so it reports 0 as the eval for forced draws
- [x] Stop search if it is taking way too long - needs threading?
- [ ] [Ronald de Man's ideas on Scoring Root Moves](https://www.chessprogramming.org/Ronald_de_Man#ScoringRootMoves)
  - Need to be careful around draws. I think I need to not apply randomness in range of a draw.
- [x] Delay move legality check until move is searched. Move ordering will take longer but will remove a make + unmake for every move
- [ ] Adjust move history immediately after searching it instead of storing a list to wait for a fail high
- [x] Avoid allocating memory during move generation
- [x] Try unstable sorting moves
- [x] Maybe done? Check for threefold repetition before using best move from tt at root
- [x] Skip rechecking the hash move in quiescense search
