# First
* Arch: (768 -> 16)x2 -> 1
* Data: 32M positions (filtered) @ 5K hard nodes (oops) self-generated with HCE. Opening: 4 random moves (per side) from startpos.
* Data filtering: Exclude games where score is >= 3.0 after opening + default Viri filtering
* Activation: screlu
* WDL: Constant 0.75
* LR: 0.001, divide by 10 every 18 superbatches
* Superbatches: 40
* Optimiser: AdamW
* QA: 255, QB: 64, Scale: 400

```
Results of engine1 vs engine2 (8+0.08, 1t, 16MB, UHO_Lichess_4852_v1.epd):
Elo: 77.11 +/- 26.92, nElo: 113.63 +/- 38.31
LOS: 100.00 %, DrawRatio: 39.24 %, PairsRatio: 2.69
Games: 316, Wins: 147, Losses: 78, Draws: 91, Points: 192.5 (60.92 %)
Ptnml(0-2): [0, 26, 62, 45, 25], WL/DD Ratio: 5.20
LLR: 2.91 (100.5%) (-2.25, 2.89) [0.00, 10.00]
```

# Second
* WDL: Constant 0.4

```
Results of engine1 vs engine2 (8+0.08, 1t, 16MB, UHO_Lichess_4852_v1.epd):
Elo: 58.72 +/- 21.83, nElo: 88.90 +/- 32.39
LOS: 100.00 %, DrawRatio: 39.82 %, PairsRatio: 2.41
Games: 442, Wins: 160, Losses: 86, Draws: 196, Points: 258.0 (58.37 %)
Ptnml(0-2): [4, 35, 88, 71, 23], WL/DD Ratio: 0.96
LLR: 2.90 (100.2%) (-2.25, 2.89) [0.00, 10.00]
```

# Third
* WDL: Linear from 0.4 to 0.7

```
Results of engine1 vs engine2 (8+0.08, 1t, 16MB, UHO_Lichess_4852_v1.epd):
Elo: -22.45 +/- 17.30, nElo: -32.50 +/- 24.97
LOS: 0.54 %, DrawRatio: 39.52 %, PairsRatio: 0.72
Games: 744, Wins: 190, Losses: 238, Draws: 316, Points: 348.0 (46.77 %)
Ptnml(0-2): [28, 103, 147, 77, 17], WL/DD Ratio: 1.16
LLR: -2.29 (-101.7%) (-2.25, 2.89) [0.00, 10.00]
```

# Fourth
* LR: Cosine decay from 0.001 to 0.001 * 0.3^5

It seems very likely this does gain some elo, but training a new network is very fast right now and this will probably gain more later. Didn't feel worth it to keep the SPRT running.

VS Second
```
giving up, it seems it is better though. will revisit.
Results of engine1 vs engine2 (8+0.08, 1t, 16MB, UHO_Lichess_4852_v1.epd):
Elo: 1.96 +/- 6.55, nElo: 2.99 +/- 10.02
LOS: 72.09 %, DrawRatio: 41.20 %, PairsRatio: 1.05
Games: 4616, Wins: 1256, Losses: 1230, Draws: 2130, Points: 2321.0 (50.28 %)
Ptnml(0-2): [107, 554, 951, 598, 98], WL/DD Ratio: 0.94
LLR: -0.77 (-34.0%) (-2.25, 2.89) [0.00, 10.00]
```

# Fennec
* Arch: (768 -> 32)x2 -> 1

VS Second
```
Results of engine1 vs engine2 (8+0.08, 1t, 16MB, UHO_Lichess_4852_v1.epd):
Elo: 93.70 +/- 26.94, nElo: 143.83 +/- 39.32
LOS: 100.00 %, DrawRatio: 35.33 %, PairsRatio: 4.11
Games: 300, Wins: 132, Losses: 53, Draws: 115, Points: 189.5 (63.17 %)
Ptnml(0-2): [1, 18, 53, 57, 21], WL/DD Ratio: 1.65
LLR: 2.91 (100.6%) (-2.25, 2.89) [0.00, 10.00]
```

# Kit
* Data: Add 36M positions (filtered) @ 5K soft nodes self-generated with Fennec

```
Results of engine1 vs engine2 (8+0.08, 1t, 16MB, UHO_Lichess_4852_v1.epd):
Elo: 53.77 +/- 20.14, nElo: 84.23 +/- 31.02
LOS: 100.00 %, DrawRatio: 41.49 %, PairsRatio: 2.44
Games: 482, Wins: 181, Losses: 107, Draws: 194, Points: 278.0 (57.68 %)
Ptnml(0-2): [5, 36, 100, 80, 20], WL/DD Ratio: 1.56
LLR: 2.95 (102.0%) (-2.25, 2.89) [0.00, 10.00]
```

# Swift
* Arch: (768 -> 64)x2 -> 1

```
Results of engine1 vs engine2 (8+0.08, 1t, 16MB, UHO_Lichess_4852_v1.epd):
Elo: 82.73 +/- 25.09, nElo: 127.06 +/- 37.04
LOS: 100.00 %, DrawRatio: 37.28 %, PairsRatio: 3.82
Games: 338, Wins: 140, Losses: 61, Draws: 137, Points: 208.5 (61.69 %)
Ptnml(0-2): [3, 19, 63, 64, 20], WL/DD Ratio: 1.33
LLR: 2.90 (100.3%) (-2.25, 2.89) [0.00, 10.00]
```

# Cape
* LR: Cosine decay from 0.001 to 0.001 * 0.3^5
* Superbatches: 80

It is testing two changes at once, but I feel like they make a lot of sense together

```
Results of engine1 vs engine2 (8+0.08, 1t, 16MB, UHO_Lichess_4852_v1.epd):
Elo: 12.36 +/- 8.85, nElo: 19.16 +/- 13.69
LOS: 99.69 %, DrawRatio: 41.71 %, PairsRatio: 1.19
Games: 2474, Wins: 693, Losses: 605, Draws: 1176, Points: 1281.0 (51.78 %)
Ptnml(0-2): [40, 289, 516, 327, 65], WL/DD Ratio: 0.84
LLR: 2.90 (100.3%) (-2.25, 2.89) [0.00, 10.00]
```
