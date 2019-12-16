module Base2048 (
    GameState(GameWon, GameLost, ComputerTurn, PlayerTurn),
    Move,
    newGame,
    getMoves,
    parsePlayerMove,
    playMove,
    playComputer,
    scoreBoard
) where

import Data.List(intercalate, transpose, partition)
import Data.List.Split(divvy)
import System.Random(getStdRandom, random)


size = 4
flatsize = size * size

goal = 2048

computerChoices = [2, 4]
computerProbs = [0.9, 0.1]


data Board = Board { boardArray :: [[Int]] }
            deriving Eq
data GameState = PlayerTurn Board | ComputerTurn Board
                | GameWon Board | GameLost Board
data Move = LeftMove | RightMove | UpMove | DownMove
            deriving Eq

instance Show GameState where
    show (PlayerTurn b) = "Player to Move.\n" ++ show b
    show (ComputerTurn b) = "Computer to Move.\n" ++ show b
    show (GameWon b) = "Player Won the Game!\n" ++ show b
    show (GameLost b) = "Player Lost the Game!\n" ++ show b

instance Show Board where
    show (Board b) = "\n" ++ (intercalate "\n" $ map showBoardRow b) ++ "\n"
      where showBoardRow r = intercalate "\t" $ map show r

instance Show Move where
    show LeftMove = "Left"
    show RightMove = "Right"
    show UpMove = "Up"
    show DownMove = "Down"


newGame :: IO GameState
newGame = playComputer blankgame
  where blankgame = ComputerTurn $ makeBoard $ replicate flatsize 0
        makeBoard flatlist = Board $ divvy size size flatlist


getMoves :: GameState -> [(Move, Board)]
getMoves (PlayerTurn board) = validPlayerMoves board
getMoves _ = error "Cannot get moves because it's not the player's turn"

validPlayerMoves :: Board -> [(Move, Board)]
validPlayerMoves board = filter ((/= board) . snd) $ zip moves boards
  where moves = [LeftMove, RightMove, UpMove, DownMove]
        boards = map (doMove board) moves

parsePlayerMove :: String -> Maybe Move
parsePlayerMove "L" = Just LeftMove
parsePlayerMove "R" = Just RightMove
parsePlayerMove "U" = Just UpMove
parsePlayerMove "D" = Just DownMove
parsePlayerMove _ = Nothing


playMove :: GameState -> Move -> IO GameState
playMove (PlayerTurn board) move
    | goalreached  = print move >>= \_ -> return $ GameWon board'
    | otherwise    = print move >>= \_ -> return $ ComputerTurn board'
  where board' = doMove board move
        goalreached = any (>= goal) $ concat $ boardArray board'
playMove_ _ = error "Cannot play move because it's not the player's turn"


-- Todo: cleanup and speed up with bitvectors
doMove :: Board -> Move -> Board
doMove (Board board) LeftMove = Board board'
      where board' = map (take size . (++ repeat 0) . merge . filter (/= 0)) board
            merge (x1:x2:xs)
              | x1 == x2 = (x1 * 2) : merge xs
              | otherwise = x1 : merge (x2 : xs)
            merge l = l
-- todo: in the following make more pretty by acting on the Board "box" directly somehow (i.e get rid of boardArray)

doMove (Board board) RightMove = Board board'
      where board' = map reverse $ boardArray $ doMove (Board $ map reverse board) LeftMove
doMove (Board board) UpMove = Board board'
      where board' = transpose $ boardArray $ doMove (Board $ transpose board) LeftMove
doMove (Board board) DownMove = Board board'
      where board' = transpose $ boardArray $ doMove (Board $ transpose board) RightMove

-- Todo: clean up to use actual moves
playComputer :: GameState -> IO GameState
playComputer (ComputerTurn board)
    = do board' <- doComputerMove board
         if null $ validPlayerMoves board'
         then return $ GameLost board'
         else return $ PlayerTurn board'
playComputer _ = error "Cannot play computer move because it's not the computer's turn"

doComputerMove :: Board -> IO Board
doComputerMove board = getStdRandom random >>=
    \r -> return $ snd $ head $ filter ((> r) . fst) $ culmProbs
  where culmProbs = allComputerMoveCulmProbs board

allComputerMoveProbs :: Board -> [(Double, Board)]
allComputerMoveProbs (Board board) = zip probs $ map Board boards'
  where flatboard = concat board
        flatboards = replicate (flatsize * length(computerChoices)) flatboard
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

allComputerMoveCulmProbs :: Board -> [(Double, Board)]
allComputerMoveCulmProbs board
    = scanl1 (\(p, b) (p', b') -> (p + p', b')) $ allComputerMoveProbs board

allComputerMoves :: Board -> [Board]
allComputerMoves board = map snd $ allComputerMoveProbs board


scoreBoard :: Board -> Int
scoreBoard (Board b) = 2^numEmpty + sumPow + monoScore - smoothPen
  where numEmpty = sum $ map (length . filter (0 ==)) b
        sumPow = sum $ map (sum . map (flip (^) 2)) b
        smoothPen = sumOverDirs (sum . map (sum . (map abs) . offsetDiffs)) b
        monoScore :: Int
        monoScore = sumOverDirs (maximum . (map sum) . transpose . (map monoScores)) b
        monoScores :: [Int] -> [Int]
        monoScores x = map (abs . sum) $ lToT $ partition (0<) $ offsetDiffs x
        offsetDiffs :: [Int] -> [Int]
        offsetDiffs l = zipWith (-) l $ tail l
        sumOverDirs :: ([[Int]] -> Int) -> [[Int]] -> Int
        sumOverDirs f b' = sum $ map f [b', transpose b']
        lToT (x,y) = [x,y]