module GameAgents where

import Base2048

simplePlayer :: GameState -> IO GameState
simplePlayer g = playMove g $ fst $ head $ getMoves g

alphaBetaPlayer :: GameState -> IO GameState
alphaBetaPlayer g = playMove g $ fst $ head $ getMoves g

fullParallelPlayer :: GameState -> IO GameState
fullParallelPlayer g = playMove g $ fst $ head $ getMoves g

mixedParallelPlayer :: GameState -> IO GameState
mixedParallelPlayer g = playMove g $ fst $ head $ getMoves g

interactivePlayer :: GameState -> IO GameState
interactivePlayer g = do
    putStrLn "Choose Move (L, R, U, D)"
    m <- parsePlayerMove <$> getLine
    case m of
        Just m' -> do
            if m' `elem` (map fst $ getMoves g)
            then playMove g m'
            else getAnotherMove
        Nothing -> getAnotherMove
  where getAnotherMove = do putStrLn "Move invalid! Choose another move."
                            interactivePlayer g