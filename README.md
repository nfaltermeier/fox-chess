# Fox chess
:3

A UCI chess engine

## Ideas of features to add
- [x] Needs some help in endgames. It lost KQK because it wouldn't move the king out of the center to support the queen. Greater depth helps with this.
  - [ ] Still needs help with harder endgames
- [ ] Avoid draws due to insufficient mating material
- [ ] [Consider checks during quiesence search](https://www.chessprogramming.org/Quiescence_Search#Checks)
- [x] [Delta pruning](https://www.chessprogramming.org/Delta_Pruning)
- [x] [Transposition table](https://www.chessprogramming.org/Transposition_Table)
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
