-- stack install split
-- stack install random

import Data.List(intercalate, transpose)
import Data.List.Split(divvy)
import System.Random(getStdRandom, random)

type MoveCount = Int
data Board = Board { boardArray :: [[Int]] }
             deriving Eq
data GameState = PlayerTurn | ComputerTurn | GameWon | GameLost
data Game = Game GameState MoveCount Board
data Move = LeftMove | RightMove | UpMove | DownMove
            deriving Eq

instance Show Game where
  show (Game gs mc b) = show gs ++ "\nMove Count: " ++ show mc ++ "\n" ++ show b

instance Show GameState where
  show PlayerTurn = "Player to Move."
  show ComputerTurn = "Computer to Move."
  show GameWon = "Player Won the Game!"
  show GameLost = "Player Lost the Game!"

instance Show Board where
  show (Board b) = "\n" ++ (intercalate "\n" $ map showBoardRow b) ++ "\n"
    where showBoardRow r = intercalate "\t" $ map show r

instance Show Move where
  show LeftMove = "Left"
  show RightMove = "Right"
  show UpMove = "Up"
  show DownMove = "Down"



size = 4
flatsize = size * size

goal = 2048

computerChoices = [2, 4]
computerProbs = [0.9, 0.1]


-- Functions to be Exposed

newGame :: IO Game
newGame = playComputerMove blankgame
    where blankgame = Game ComputerTurn 0 $ makeBoard $ replicate flatsize 0

playTurn :: Game -> Move -> IO Game
playTurn game move = playComputerMove $ playPlayerMove game move

playPlayerMove :: Game -> Move -> Game
playPlayerMove (Game PlayerTurn movecount board) move
  | goalreached  = Game GameWon (movecount + 1) board'
  | otherwise    = Game ComputerTurn (movecount + 1) board'
  where board' = doPlayerMove board move
        goalreached = any (>= goal) $ concat $ boardArray board'
playPlayerMove _ _ = error "Cannot play player move because it is not the player's turn"

playComputerMove :: Game -> IO Game
playComputerMove (Game ComputerTurn movecount board)
    = do board' <- doComputerMove board
         if null $ allPlayerMoves board'
         then return $ Game GameLost movecount board'
         else return $ Game PlayerTurn movecount board'

playComputerMove _
    = error "Cannot play computer move because it is not the computer's turn"

getPlayerMoves :: Game -> [(Move, Board)]
getPlayerMoves (Game PlayerTurn _ board) = allPlayerMoves board
getPlayerMoves _ = error "Cannot get player moves because it is not the player's turn"

getComputerMoves :: Game -> [Board]
getComputerMoves (Game ComputerTurn _ board) = allComputerMoves board
getComputerMoves _ = error "Cannot get computer moves because it is not the computer's turn"


-- Functions to be used for testing

makeBoard :: [Int] -> Board
makeBoard flatlist = Board $ divvy size size flatlist

makeGame :: [Int] -> Game
makeGame flatlist = Game PlayerTurn 9999 $ makeBoard flatlist



-- Functions to be kept internal

allPlayerMoves :: Board -> [(Move, Board)]
allPlayerMoves board = filter ((/= board) . snd) $ zip moves boards
  where moves = [LeftMove, RightMove, UpMove, DownMove]
        boards = map (doPlayerMove board) moves

allComputerMoves :: Board -> [Board]
allComputerMoves board = map snd $ allComputerMoveProbs board


doPlayerMove :: Board -> Move -> Board
doPlayerMove (Board board) LeftMove
    = Board board'
    where board' = map (take size . (++ repeat 0) . merge . filter (/= 0)) board
          merge (x1:x2:xs)
            | x1 == x2 = (x1 * 2) : merge xs
            | otherwise = x1 : merge (x2 : xs)
          merge l = l

-- todo: in the following make more pretty by acting on the Board "box" directly somehow (i.e get rid of boardArray)

doPlayerMove (Board board) RightMove
    = Board board'
    where board' = map reverse $ boardArray $ doPlayerMove (Board $ map reverse board) LeftMove
doPlayerMove (Board board) UpMove
    = Board board'
    where board' = transpose $ boardArray $ doPlayerMove (Board $ transpose board) LeftMove
doPlayerMove (Board board) DownMove
    = Board board'
    where board' = transpose $ boardArray $ doPlayerMove (Board $ transpose board) RightMove

doComputerMove :: Board -> IO Board
doComputerMove board
    = getStdRandom random >>=
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


parseMove :: String -> Move
parseMove "L" = LeftMove
parseMove "R" = RightMove
parseMove "U" = UpMove
parseMove "D" = DownMove
parseMove _ = error "Unknown move"


-- Interactive 2048!

main :: IO ()
main = do putStrLn "Welcome to 2048!"
          g <- newGame
          interactiveGame g

interactiveGame :: Game -> IO ()
interactiveGame g@(Game PlayerTurn _ _)
    = do print g
         putStrLn "Choose Move (L, R, U, D)"
         m <- parseMove <$> getLine
         if m `elem` (map fst $ getPlayerMoves g)
         then
           do g' <- playTurn g m
              interactiveGame g'
         else
           do putStrLn "Move invalid! Choose another move."
              interactiveGame g

interactiveGame g@(Game GameWon _ _)
    = print g >> putStrLn "Congratulations!"

interactiveGame g@(Game GameLost _ _)
    = print g >> putStrLn "Play again soon!"

interactiveGame _
    = error "Invalid game state for interactiveGame: Is computer's turn"
