module Base2048 (
    GameState(GameWon, GameLost, ComputerTurn, PlayerTurn),
    Move,
    newGame,
    scoreGame,
    nextStates,
    getPlayerMoves,
    parsePlayerMove,
    playComputer,
    getComputerState
) where

import Data.List(intercalate, transpose, partition)
import Data.List.Split(divvy)
import System.Random(getStdRandom, random)
import Control.Monad(guard)


size = 4
goal = 2048

computerChoices = [2, 4]
computerProbs = [0.9, 0.1]


type Board = [[Int]]
data GameState = PlayerTurn Board | ComputerTurn Board
                | GameWon Board | GameLost Board
data Move = LeftMove | RightMove | UpMove | DownMove
            deriving Eq

instance Show GameState where
    show (PlayerTurn b) = "Player to Move.\n" ++ printBoard b
    show (ComputerTurn b) = "Computer to Move.\n" ++ printBoard b
    show (GameWon b) = "Player Won the Game!\n" ++ printBoard b
    show (GameLost b) = "Player Lost the Game!\n" ++ printBoard b

printBoard :: Board -> String
printBoard b = "\n" ++ (intercalate "\n" $ map showBoardRow b) ++ "\n"
    where showBoardRow r = intercalate "\t" $ map show r


newGame :: IO GameState
newGame = playComputer $ ComputerTurn [replicate size 0 | _ <- replicate size 0]

{-
    Heuristic for how good a state is for the player
    Rewards empty squares, higher value tiles, and evenly increasing rows/cols
    Penalizes large gaps in value between neighboring tiles
-}
scoreGame :: GameState -> Double
scoreGame (GameLost _) = -1 * (read "Infinity")::Double
scoreGame (GameWon _) = (read "Infinity")::Double
scoreGame (PlayerTurn board) = scoreBoard board
scoreGame (ComputerTurn board) = scoreBoard board

scoreBoard :: Board -> Double
scoreBoard b = 2^numEmpty + 0.5*sumPow + monoScore + snakeBias - 0.65*smoothPen
  where numEmpty = sum $ map (length . filter (0 ==)) b
        sumPow = fromIntegral $ sum $ map (sum . map (flip (^) 2)) b
        smoothPen = fromIntegral $
            sumOverDirs (sum . map (sum . (map abs) . offsetDiffs)) b
        monoScore = fromIntegral $
            sumOverDirs (maximum . (map sum) . transpose . (map monoScores)) b
        monoScores x = map (abs . sum) $ tupToL $ partition (0<) $ offsetDiffs x
        offsetDiffs l = zipWith (-) l $ tail l
        sumOverDirs f b' = sum $ map f [b', transpose b']
        tupToL (x,y) = [x,y]
        snakeBias = fromIntegral $ weightSum $ concat $ flipAltRows
        weightSum l = sum $ zipWith (*) (map (flip (^) 2) [0..]) l
        flipAltRows = zipWith ($) (cycle [id, reverse]) b

-- Wrapper to abstract turns and moves away from agents
nextStates :: GameState -> [GameState]
nextStates (PlayerTurn b) = map snd $ getPlayerMoves b
nextStates (ComputerTurn b) = getComputerMoves b
nextStates _ = []


getPlayerMoves :: Board -> [(Move, GameState)]
getPlayerMoves board = do
    move <- moves
    let successor = doPlayerMove board move
        state = getState successor
    guard $ successor /= board
    return (move, state)
  where moves = [LeftMove, RightMove, UpMove, DownMove]
        getState b | winState b = GameWon b
                   | otherwise  = ComputerTurn b
        winState b = any (>= goal) $ concat b

-- fourfold symmetry of board, so implemented actual transformation only once
doPlayerMove :: Board -> Move -> Board
doPlayerMove b LeftMove = board'
  where board' = map (take size . (++ repeat 0) . merge . filter (/= 0)) b
        merge (x1:x2:xs)
            | x1 == x2 = (x1 * 2) : merge xs
            | otherwise = x1 : merge (x2 : xs)
        merge l = l
doPlayerMove b RightMove = map reverse $ doPlayerMove (map reverse b) LeftMove
doPlayerMove b UpMove = transpose $ doPlayerMove (transpose b) LeftMove
doPlayerMove b DownMove = transpose $ doPlayerMove (transpose b) RightMove

-- Enables interactive player
parsePlayerMove :: String -> Maybe Move
parsePlayerMove "L" = Just LeftMove
parsePlayerMove "R" = Just RightMove
parsePlayerMove "U" = Just UpMove
parsePlayerMove "D" = Just DownMove
parsePlayerMove _ = Nothing


getComputerMoves :: Board -> [GameState]
getComputerMoves board = map getComputerState newBoards
  where newBoards = map snd $ allComputerMoveProbs board

playComputer :: GameState -> IO GameState
playComputer (ComputerTurn b) = doComputerMove b >>=
    \b' -> return $ getComputerState b'
playComputer _ = error "It's not the Computer's turn."

getComputerState :: Board -> GameState
getComputerState b | loseState b = GameLost b
                   | otherwise  = PlayerTurn b
  where loseState b' = gridMin b' > 0
            && minimum (map diffMin [b', transpose b']) > 0
        diffMin b' = minimum $ map (minimum . (map abs) . offsetDiffs) b'
        offsetDiffs l = zipWith (-) l $ tail l
        gridMin g = minimum (map minimum g)

doComputerMove :: Board -> IO Board
doComputerMove board = getStdRandom random >>=
    \r -> return $ snd $ head $ filter ((> r) . fst) $ cumulativeProbs
  where
    cumulativeProbs = scanl1 addCumulative $ allComputerMoveProbs board
    addCumulative = \(p, b) (p', b') -> (p + p', b')

allComputerMoveProbs :: Board -> [(Double, Board)]
allComputerMoveProbs board = zip probs boards'
  where flatboard = concat board
        flatboards = replicate (size^2 * length(computerChoices)) flatboard
        flatboards' = zipWith combineDelta flatboards deltas
        uniqueflatboards' = filter (/= flatboard) flatboards'
        boards' = map (divvy size size) uniqueflatboards'

        probs = cycle $ map (* (lenratio computerChoices uniqueflatboards')) computerProbs

        deltas = interleave $ map ndeltas computerChoices
        ndeltas n = (n : repeat 0) : (map (0:) $ ndeltas n)

        lenratio a b = (fromIntegral $ length a) / (fromIntegral $ length b)

        interleave = concat . transpose
        combineDelta flatboard delta = zipWith combine flatboard delta
        combine 0 new = new
        combine orig _ = orig