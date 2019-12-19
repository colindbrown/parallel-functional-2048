import Base2048
import GameAgents
import System.Exit(die, exitSuccess, exitFailure)
import System.Environment(getArgs, getProgName)
import System.IO(withFile, IOMode(ReadMode), hGetContents)

main :: IO ()
main = do args <- getArgs
          case args of
            [maxdepth, playerType] -> do
                withFile "boards.txt" ReadMode (\h -> do
                    contents <- hGetContents h
                    let solve = player (read maxdepth)
                    solvedBoards <- sequence $ map (solve . PlayerTurn . read) $ lines contents
                    mapM_ (putStrLn . show) solvedBoards)
                      where player = case playerType of
                                "simple" -> simplePlayer
                                "ab" -> alphaBetaPlayer
                                "par" -> fullParallelPlayer
                                "mixed" -> ybcwPlayer
                                p -> error $ p ++ " is not a valid player type"
            _ -> do pn <- getProgName
                    die $ "Usage: " ++ pn ++ " <max depth> <player type>"