module Main where

import Base2048
import GameAgents
import System.Exit(die, exitSuccess, exitFailure)
import System.Environment(getArgs, getProgName)

main :: IO ()
main = do args <- getArgs
          case args of
            [maxdepth, playerType] -> playGame (read maxdepth) player
              where player = case playerType of
                        "int" -> interactivePlayer
                        "simple" -> simplePlayer
                        "ab" -> alphaBetaPlayer
                        "par" -> fullParallelPlayer
                        "mixed" -> ybcwPlayer
                        p -> error $ p ++ " is not a valid player type"
            _ -> do pn <- getProgName
                    die $ "Usage: " ++ pn ++ " <max depth> <player type>"

playGame :: Int -> (Int -> GameState -> IO GameState) -> IO ()
playGame maxdepth player = newGame >>= playFrom
  where playFrom g = do
            print g
            case g of
                (PlayerTurn _) -> player maxdepth g >>= playFrom
                (ComputerTurn _) -> playComputer g >>= playFrom
                (GameWon _) -> putStrLn "Congratulations!" >> exitSuccess
                (GameLost _) -> putStrLn "Try again soon!" >> exitFailure
