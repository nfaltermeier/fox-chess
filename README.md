# Fox chess
:3

A UCI chess engine

## Ideas of features to add
- [x] Needs some help in endgames. It lost KQK because it wouldn't move the king out of the center to support the queen. Greater depth helps with this.
  - [ ] Still needs help with harder endgames
- [ ] Avoid draws due to insufficient mating material
- [ ] [Consider checks during quiesence search](https://www.chessprogramming.org/Quiescence_Search#Checks)
  - [ ] [Specialized check-only move generation](https://www.chessprogramming.org/Move_Generation#Special_Generators)
  - Not sure if much can be done for check evasion without bitboards
- [x] [Delta pruning](https://www.chessprogramming.org/Delta_Pruning)
- [x] [Transposition table](https://www.chessprogramming.org/Transposition_Table)
  - [ ] Better replacement scheme. Can pack some data into part of the hash, using the index to recover the rest of the hash.
- [x] [Iterative deepening](https://www.chessprogramming.org/Iterative_Deepening)
  - [ ] [Aspiration window?](https://www.chessprogramming.org/Aspiration_Windows)
- [ ] [Null move pruning](https://www.chessprogramming.org/Null_Move_Pruning)
- [ ] [Some kind of better Move Ordering](https://www.chessprogramming.org/Move_Ordering)
  - [x] [Killer heuristic](https://www.chessprogramming.org/Killer_Heuristic)
  - [x] [History heuristic](https://www.chessprogramming.org/History_Heuristic)
  - [ ] [Late move reduction](https://www.chessprogramming.org/Late_Move_Reductions)
- [ ] Refine piece square evaluations
  - [ ] Automated tuning?
- [ ] Evaluation
  - [ ] [Consider Mobility (and negative mobility for king)](https://www.chessprogramming.org/Mobility)
  - [ ] Pawn evaluation
    - [ ] [Pawn hash table](https://www.chessprogramming.org/Pawn_Hash_Table)
    - [x] Isolated pawns
    - [ ] Doubled pawns
    - [ ] [Pawn chains](https://www.chessprogramming.org/Connected_Pawns)
    - [ ] [Passed pawns (wiki lists lots of subtypes to maybe consider)](https://www.chessprogramming.org/Passed_Pawn)
- [ ] [Use bitboards to improve move generation](https://www.chessprogramming.org/Bitboards)
- [ ] Opening book
- [ ] Endgame tablebase
- [ ] [Contempt factor](https://www.chessprogramming.org/Contempt_Factor)
  - [ ] Judge the opponent's moves based on our PV and how they change the eval like described in [this thread](https://www.talkchess.com/forum/viewtopic.php?p=531133#p531133)
- [ ] Redo mate scoring so at mate it returns the full 20000cp and then lower it as it walks up the tree. That way each node in tt will have the proper score.
- [ ] Redo something about evaluation so it reports 0 as the eval for forced draws
- [x] Stop search if it is taking way too long - needs threading?
- [ ] Always track position of kings to quickly check for move legality
- [ ] [Ronald de Man's ideas on Scoring Root Moves](https://www.chessprogramming.org/Ronald_de_Man#ScoringRootMoves)
  - Need to be careful around draws. I think I need to not apply randomness in range of a draw.
- [x] Delay move legality check until move is searched. Move ordering will take longer but will remove a make + unmake for every move
- [ ] Adjust move history immediately after searching it instead of storing a list to wait for a fail high
- [ ] Reserve space for moves during move generation
- [ ] Try unstable sorting moves
- [ ] Check for threefold repetition before using best move from tt at root
- [ ] Skip rechecking the hash move in quiescense search
