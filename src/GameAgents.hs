module GameAgents where

import Base2048
import Data.List(lookup, maximumBy, minimumBy)
import Data.Ord(comparing)
import Control.Parallel.Strategies(runEval, parMap, rpar)

inf = read "Infinity" :: Double

simplePlayer :: Int -> GameState -> IO GameState
simplePlayer maxdepth game = return $ snd $ minimax maxdepth game
  where
    minimax 0 g = (scoreGame g, g)
    minimax d g@(PlayerTurn _) = maximumBy (comparing fst) $ mmValues d g
    minimax d g@(ComputerTurn _) = minimumBy (comparing fst) $ mmValues d g
    minimax _ g = (scoreGame g, g)
    mmValues d g = [(fst $ minimax (d-1) x, x) | x <- nextStates g]

alphaBetaPlayer :: Int -> GameState -> IO GameState
alphaBetaPlayer maxdepth game = return $ snd $ minimax maxdepth game (-1*inf) inf
  where
    minimax 0 g _ _ = (scoreGame g, g)
    minimax d g@(PlayerTurn _) a b = maxFold a b d $ nextStates g
    minimax d g@(ComputerTurn _) a b = minFold a b d $ nextStates g
    minimax _ g _ _ = (scoreGame g, g)

    maxFold a' b' d [g] = (fst $ minimax (d-1) g a' b', g)
    maxFold a' b' d (g:gs) =
        let (s, _) = minimax (d-1) g a' b'; newA = max a' s
        in case newA >= b' of
            False -> maximumBy (comparing fst) [(s, g), maxFold newA b' d gs]
            True -> (s, g)

    minFold a' b' d [g] = (fst $ minimax (d-1) g a' b', g)
    minFold a' b' d (g:gs) =
        let (s, _) = minimax (d-1) g a' b'; newB = min b' s
        in case a' >= newB of
            False -> minimumBy (comparing fst) [(s, g), minFold a' newB d gs]
            True -> (s, g)

fullParallelPlayer :: Int -> GameState -> IO GameState
fullParallelPlayer maxdepth game = return $ snd $ minimax maxdepth game
  where
    minimax 0 g = (scoreGame g, g)
    minimax d g@(PlayerTurn _) = maximumBy (comparing fst) $ mmValues d g
    minimax d g@(ComputerTurn _) = minimumBy (comparing fst) $ parMMValues d g
    minimax _ g = (scoreGame g, g)
    mmValues d g = [(fst $ minimax (d-1) x, x) | x <- nextStates g]
    parMMValues d g | d > 3 = zipWith (\(s,_) x -> (s, x)) (scores d g) $ nextStates g
                    | otherwise = mmValues d g
    scores d g = parMap rpar (minimax (d-1)) (nextStates g)

-- uses a mixed "Young Brothers Can Wait Strategy"
-- fully evaluates first child for alpha/beta then uses those in paralle for others
ybcwPlayer :: Int -> GameState -> IO GameState
ybcwPlayer maxdepth game = return $ snd $ minimax (-1*inf) inf maxdepth game 
    where
    minimax _ _ 0 g= (scoreGame g, g)
    minimax a b d g@(PlayerTurn _) = maxYBCW a b d $ nextStates g
    minimax a b d g@(ComputerTurn _) = minYBCW a b d $ nextStates g
    minimax _ _ _ g = (scoreGame g, g)

    maxYBCW a' b' d (g:gs) =
        let (s, _) = minimax a' b' (d-1) g; newA = max a' s
        in case newA >= b' of
            False -> maximumBy (comparing fst) $ (s, g) : parMMValues newA b' d gs
            True -> (s, g)
    minYBCW a' b' d (g:gs) =
        let (s, _) = minimax a' b' (d-1) g; newB = min b' s
        in case a' >= newB of
            False -> minimumBy (comparing fst) $ (s, g) : parMMValues a' newB d gs
            True -> (s, g)
    parMMValues a b d gs = zipWith (\(s,_) x -> (s, x)) (scores a b d gs) gs
    scores a b d gs = parMap rpar (minimax a b (d-1)) gs

-- interactivePlayer takes a dummy Int parameter that it ignores
-- This is to match the function signature of the other agents (which take maxdepth)
interactivePlayer :: Int -> GameState -> IO GameState
interactivePlayer _ = interactivePlayer'

interactivePlayer' :: GameState -> IO GameState
interactivePlayer' g@(PlayerTurn b) = do
    putStrLn "Choose Move (L, R, U, D)"
    m <- parsePlayerMove <$> getLine
    case m of
        Just m' -> do
            case lookup m' $ getPlayerMoves b of
                Just newState -> return newState
                Nothing -> getAnotherMove
        Nothing -> getAnotherMove
  where getAnotherMove = do putStrLn "Move invalid! Choose another move."
                            interactivePlayer' g
interactivePlayer' _ = error "It's not the Player's turn"
