module Main where

import Auto2048 as Game
import GeneticSearch as Search

{-
    usage should be
    ./pf2048 -> usage message
    ./pf2048 play (filename) -> plays game with printing board, optional filename of params to use
    ./pf2048 train (filename) -> trains params using genetic search, filename of where to store learned params
-}
main :: IO ()
main = play2048

play2048 :: IO ()
play2048 = do
    score <- Game.playRound Game.defaultParameters
    putStrLn $ show score

evolveParameters :: IO ()
evolveParameters = do
    params <- Search.