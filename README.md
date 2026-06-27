# Fox chess
:3

A UCI chess engine. Intended for use with a chess GUI such as Cute Chess or Arena or interfacing with a chess server with a program like lichess-bot.

The program is available for challenge some of the time at [lichess](https://lichess.org/@/FoxChessBot). I run it on my own computer so availability is sporadic and is not guranteed.

Now uses an efficiently updatable neural network (NNUE) for evaluation, which is trained on exclusively self-generated data. The network was trained using [bullet](https://github.com/jw1912/bullet/tree/main).

## Rating
| Version | CCRL 40/15 | CCRL Blitz | Biggest additions |
|---------|------------|------------|-------------------|
| v1.2    |            | Est. 3060  | Static eval correction histories, adjusting fp and rfp, multithreading support |
| v1.1    | 2876       |            | Singular extensions, mobility eval, and fixing history using ply instead of depth |
| v1.0    | 2727       |            | |

## Uci Options
* Hash: Sets the transposition table size in MiB (Mebibytes). Must be at least 1 and will be rounded down to a power of 2. Default is 128.
* Threads: Sets the number of threads to use while searching. Must be at least 1. NUMA has not been tested for high thread counts. Default is 1.
* MultiPV: The engine will search for and print this many Principal Variations / bestmoves. Values greater than 1 make search slower but produce more accurate results. Value must be at least 1 and less than 256. Default is 1.
* Contempt: Sets the engine's draw score (in centipawns). Higher means the engine wants to avoid a draw more. Default is 0 and range is -100 to 100.
* Soft Max Nodes: When enabled and max nodes is specified (ex. `go nodes 100000`), then the engine will try to finish searching the current iteration before reporting the best move (it can search more nodes than specified). If enabled and nodes searched exceeds 20x the maximum, then it will still cancel the search. A hard nodes maximum will not be exactly followed when using multiple threads. Default is disabled (false).
* Move Overhead: Search will try to stop this many milliseconds earlier than normal. To account for overhead from network, GUI, etc. Default is 0.

## Prerequisites for Building
The MSRV is currently 1.88. Using the latest version is probably best. I'm using 1.95.0 currently.

To create a PGO optimized build (recommended) you will need the `llvm-profdata` binary which can be installed with:
```
rustup component add llvm-tools-preview
```
and you are recommended to use the cargo-pgo tool to perform the build, this can be installed with:
```
cargo install cargo-pgo
```

## Building
Fox Chess now uses neural networks for evaluation. I currently don't have any automatic download setup, so you'll have to download it yourself and put it in the `networks` folder.
You can find the networks here: https://github.com/nfaltermeier/fox-chess-nets/releases. If you don't do this, you'll get a compilation error such as 'error: couldn't read `src\../networks/first.nnue`: The system cannot find the file specified.'

All of your CPU's supported features will automatically be enabled for the best performance. To perform the build, you can use
```
cargo pgo run -- bench
cargo pgo optimize
```
To skip PGO optimization, although this is not recommended, just run
```
cargo build -r
```

### Note for Ryzen CPUs below 5000 series (zen 1 and zen 2)
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

If you want to build a fully portable executable on x86_64-pc-windows-msvc then you will need to explicitly pass `x86-64-v1` for the target. Most computers will support `x86-64-v3` or at least `x86-64-v2` though.

#### AVX2 without PEXT
This note is mostly for myself, but to create an optimized build without any code changes for Ryzen 1000 and 3000 series CPUs, use
```
RUSTFLAGS=-Ctarget-cpu=x86-64-v3 cargo pgo run -- --no-default-features -- bench
RUSTFLAGS=-Ctarget-cpu=x86-64-v3 cargo pgo optimize build -- --no-default-features
```

## Credits
Many thanks to the [Chess Programming Wiki](https://www.chessprogramming.org) for explaining the basic and advanced concepts of creating a chess engine

The Engine Programming Discord for interesting discussions and ideas

Other open source chess engines
