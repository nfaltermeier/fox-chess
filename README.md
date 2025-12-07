# Fox chess
:3

A UCI chess engine. Intended for use with a chess GUI such as Cute Chess or Arena or interfacing with a chess server with a program like lichess-bot.

The program is available for challenge some of the time at [lichess](https://lichess.org/@/FoxChessBot). I run it on my own computer so availability is sporadic and is not guranteed.


## Uci Options
* Hash: transposition table size in MiB (Mebibytes). Must be at least 1 and will be rounded down to a power of 2.
* MultiPV: The engine will search for and print this many Principal Variations / bestmoves. Values greater than 1 make search slower but produce more accurate results. Value must be at least 1 and less than 256.

## Prerequisites for Building
The MSRV is currently 1.88.

To create a PGO optimized build (recommended) you will need the `llvm-profdata` binary which can be installed with:
```
rustup component add llvm-tools-preview
```
and you are recommended to use the cargo-pgo tool to perform the build, this can be installed with:
```
cargo install cargo-pgo
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
