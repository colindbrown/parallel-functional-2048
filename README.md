# Parallel Functional 2048
*Colin Brown and Jonathan Rich*
2048-playing agent implemented using adversarial search with alpha-beta pruning

## Usage
./pf2048 play (filename)  - Play and display a game of 2048, using saved hyperparameters if filename provided

## Todos
### Main.hs
- [ ] Print usage message if neither play nor train specified
- [ ] Get parameters from file if play filename specified (checking for invalid file)
- [ ] Open handle for train from filename if provided or default (overwrite existing)
- [ ] main :: IO () # either calls playRound or trainParams

### Auto2048.hs
- [ ] Board type [Int]
- [ ] Params type [Int]
- [ ] Heuristic type (Board -> Int)
- [ ] GameState data Computer Board | Player Board
- [ ] Move data Left () | Right () | Up () | Down ()
- [ ] makeHeuristic :: Params -> Heuristic
- [ ] playRound :: Bool -> Heuristic -> IO (Int) # bool controls if board printed while playing
- [ ] computerMove :: GameState -> GameState
- [ ] playerMove :: GameState -> Heuristic -> GameState
- [ ] printBoard :: Board -> IO ()
- [ ] placeTile :: Board -> Int -> Int -> Board
- [ ] makeMove :: Board -> Move -> Board
- [ ] minimax :: GameState -> Heuristic -> Int -> Int -> Int ->  (Int, Move) # 3 ints are remaining depth, alpha, beta

### Parallel implementation
- [ ] full parallel
- [ ] mixed parallel w/ some branch in full, then rest in parallel
- [ ] truncated search w/ heuristic values to speed up

### Speedups
- [ ] bitvectors
