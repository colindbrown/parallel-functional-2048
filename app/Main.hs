module Main where

import Base2048
import GameAgents
import System.Exit(die)
import System.Environment(getArgs, getProgName)

main :: IO ()
main = do args <- getArgs
          case args of
            [playerType] -> playGame player
              where player = case playerType of
                        "int" -> interactivePlayer
                        "simple" -> simplePlayer
                        "ab" -> alphaBetaPlayer
                        p -> error $ p ++ " is not a valid player type"
            _ -> do pn <- getProgName
                    die $ "Usage: " ++ pn ++ " <player type>"

playGame :: (GameState -> IO GameState) -> IO ()
playGame player = newGame >>= playFrom
  where playFrom g = do
            print g
            case g of
                (PlayerTurn _) -> player g >>= \g' -> playFrom g'
                (ComputerTurn _) -> playComputer g >>= \g' -> playFrom g'
                (GameWon _) -> putStrLn "Congratulations!"
                (GameLost _) -> putStrLn "Try again soon!"