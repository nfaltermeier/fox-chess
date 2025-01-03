# Fox chess
:3

A UCI chess engine

## Ideas of features to add
- [x] Needs some help in endgames. It lost KQK because it wouldn't move the king out of the center to support the queen. Greater depth helps with this.
  - [ ] Still needs help with harder endgames
- [ ] Avoid draws due to insufficient mating material
- [x] [Delta pruning](https://www.chessprogramming.org/Delta_Pruning)
- [x] [Transposition table](https://www.chessprogramming.org/Transposition_Table)
- [x] [Iterative deepening](https://www.chessprogramming.org/Iterative_Deepening)
  - [ ] [Aspiration window?](https://www.chessprogramming.org/Aspiration_Windows)
- [ ] [Null move pruning](https://www.chessprogramming.org/Null_Move_Pruning)
- [ ] [Some kind of better Move Ordering](https://www.chessprogramming.org/Move_Ordering)
  - [ ] [History heuristic](https://www.chessprogramming.org/History_Heuristic)
  - [ ] [Late move reduction](https://www.chessprogramming.org/Late_Move_Reductions)
- [ ] Refine piece square evaluations
  - [ ] Automated tuning?
- [ ] [Evaluation: Consider Mobility (and negative mobility for king)](https://www.chessprogramming.org/Mobility)
- [ ] [Evaluation: Pawn chains](https://www.chessprogramming.org/Connected_Pawns)
- [ ] [Use bitboards to improve move generation](https://www.chessprogramming.org/Bitboards)
- [ ] Opening book
- [ ] Endgame tablebase
- [ ] [Contempt factor](https://www.chessprogramming.org/Contempt_Factor)
