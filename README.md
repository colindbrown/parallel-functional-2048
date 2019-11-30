# Parallel Functional 2048
*Colin Brown and Jonathan Rich*
2048-playing agent implemented using adversarial search with alpha-beta pruning
Heuristic hyperparameter tuning using genetic search

## Usage
./pf2048 play (filename)  - Play and display a game of 2048, using saved hyperparameters if filename provided

./pf2048 train (filename) - Train and save hyperparameters, output file named filename if specified

## Todos
### Main.hs
- [ ] Print usage message if neither play nor train specified
- [ ] Get parameters from file if play filename specified (checking for invalid file)
- [ ] Open handle for train from filename if provided or default (overwrite existing)
- [ ] main :: IO () # either calls playRound or trainParams

### Auto2048.hs
Minimax function eventually done in parallel
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

### GeneticSearch.hs
imports Params, makeHeuristic, and playRound from Auto2048
- [ ] trainParams :: Handle -> IO ()
- [ ] evalParams :: Params -> Int # (playRound False) . makeHeuristic
- [ ] saveBest :: Handle -> Params -> IO () # write to temp, delete old, change name

Also needs some sort of current store of param sets, method for generating new, method of integrating evaluated params (printing out log messages), detecting convergence and ending.

Should be able to be interrupted and always have valid saved params file.