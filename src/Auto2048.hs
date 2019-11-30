module Auto2048
    ( defaultParameters,
      playRound
    ) where

defaultParameters :: [Int]
defaultParameters = []

{- Takes a set of parameters and returns an IO (Int) monad, where the Int is the final score and the IO contains the printed board output if Bool=True, otherwise empty-}
playRound :: [Int] -> IO (Int)
playRound _ = do
    putStrLn "This would be the board"
    return 0