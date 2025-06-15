# Fox chess
:3

A UCI chess engine

When targetting the x86_64-pc-windows-msvc triple all native CPU features will be enabled by default.
To build a portable executable, specify an appropriate target cpu with `-Ctarget-cpu=<target>`.

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
- [X] [Contempt factor](https://www.chessprogramming.org/Contempt_Factor)
  - [X] Judge the opponent's moves based on our PV and how they change the eval like described in [this thread](https://www.talkchess.com/forum/viewtopic.php?p=531133#p531133)
- [x] Redo mate scoring so at mate it returns the full 20000cp and then lower it as it walks up the tree. That way each node in tt will have the proper score.
- [x] Redo something about evaluation so it reports 0 as the eval for forced draws
- [x] Stop search if it is taking way too long - needs threading?
- [ ] [Ronald de Man's ideas on Scoring Root Moves](https://www.chessprogramming.org/Ronald_de_Man#ScoringRootMoves)
  - Need to be careful around draws. I think I need to not apply randomness in range of a draw.
- [x] Delay move legality check until move is searched. Move ordering will take longer but will remove a make + unmake for every move
- [ ] Adjust move history immediately after searching it instead of storing a list to wait for a fail high
- [ ] Reserve space for moves during move generation
- [x] Try unstable sorting moves
- [x] Maybe done? Check for threefold repetition before using best move from tt at root
- [x] Skip rechecking the hash move in quiescense search
