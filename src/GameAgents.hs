module GameAgents (
    simplePlayer,
    fullParallelPlayer,
    alphaBetaPlayer,
    ybcwPlayer,
    interactivePlayer
) where

import Base2048
import Data.List(lookup, maximumBy, minimumBy)
import Data.Ord(comparing)
import Control.Parallel.Strategies(runEval, parMap, rpar)

inf = read "Infinity" :: Double
depthLimit = 2 :: Int

-- types to clean up function signatures
type MMResult = (Double, GameState)
type Minimax = Int -> GameState -> MMResult
type ABMinimax = Double -> Double -> Int -> GameState -> MMResult
type ABMap = Double -> Double -> Int -> [GameState] -> [MMResult]

fstMax = maximumBy (comparing fst) :: [MMResult] -> MMResult
fstMin = minimumBy (comparing fst) :: [MMResult] -> MMResult

simplePlayer :: Int -> GameState -> IO GameState
simplePlayer maxdepth game = return $ snd $ seqMM maxdepth game

fullParallelPlayer :: Int -> GameState -> IO GameState
fullParallelPlayer maxdepth game = return $ snd $ parMM maxdepth game

alphaBetaPlayer :: Int -> GameState -> IO GameState
alphaBetaPlayer maxdepth game = return $ snd $ abMM (-1*inf) inf maxdepth game

-- uses a mixed "Young Brothers Can Wait Strategy"
-- fully evaluates first child for alpha/beta then parallel searches remaining
ybcwPlayer :: Int -> GameState -> IO GameState
ybcwPlayer maxdepth game = return $ snd $ ybcwMM (-1*inf) inf maxdepth game


seqMM :: Minimax
seqMM 0 g                  = (scoreGame g, g)
seqMM d g@(PlayerTurn _)   = fstMax $ seqMMVals seqMM d $ nextStates g
seqMM d g@(ComputerTurn _) = fstMin $ seqMMVals seqMM d $ nextStates g
seqMM _ g                  = (scoreGame g, g)

parMM :: Minimax
parMM 0 g                  = (scoreGame g, g)
parMM d g@(PlayerTurn _)   = fstMax $ seqMMVals parMM d $ nextStates g
parMM d g@(ComputerTurn _) = fstMin $ parMMVals parMM seqMM d $ nextStates g
parMM _ g                  = (scoreGame g, g)

abMM :: ABMinimax
abMM _ _ 0 g                  = (scoreGame g, g)
abMM a b d g@(PlayerTurn _)   = fstMax $ maxABEval a b d $ nextStates g
abMM a b d g@(ComputerTurn _) = fstMin $ minABEval a b d $ nextStates g
abMM _ _ _ g                  = (scoreGame g, g)

ybcwMM :: ABMinimax
ybcwMM _ _ 0 g                  = (scoreGame g, g)
ybcwMM a b d g@(PlayerTurn _)   = fstMax $ maxYBCWEval a b d $ nextStates g
ybcwMM a b d g@(ComputerTurn _) = fstMin $ minYBCWEval a b d $ nextStates g
ybcwMM _ _ _ g                  = (scoreGame g, g)


-- gets minimax values of next layer lazily
seqMMVals :: Minimax -> Int -> [GameState] -> [MMResult]
seqMMVals mm d gs = [(fst $ mm (d-1) x, x) | x <- gs]

-- gets minimax values of next layer in parallel
-- uses parmm minimax function in parallel if above a certain depth
-- otherwise finishes tree using seqmm minimax function
parMMVals :: Minimax -> Minimax -> Int -> [GameState] -> [MMResult]
parMMVals parmm seqmm d gs | d > depthLimit  = zipWith (,) scores gs
                           | otherwise       = seqMMVals seqmm d gs
  where scores = map fst $ parMap rpar (parmm (d-1)) gs

-- functions implementing the AB updates and pruning
maxABFold :: ABMinimax -> ABMap
    -> Double -> Double -> Int -> [GameState] -> [MMResult]
maxABFold mm _ a b d [g]                   = [(s, g)]
  where s = fst $ mm a b (d-1) g
maxABFold mm eval a b d (g:gs) | newA >= b = [(s, g)]
                               | otherwise = (s, g) : eval newA b d gs
  where s = fst $ mm a b (d-1) g
        newA = max a s

minABFold :: ABMinimax -> ABMap
    -> Double -> Double -> Int -> [GameState] -> [MMResult]
minABFold mm _ a b d [g]                   = [(s, g)]
  where s = fst $ mm a b (d-1) g
minABFold mm eval a b d (g:gs) | a >= newB = [(s, g)]
                               | otherwise = (s, g) : eval a newB d gs
  where s = fst $ mm a b (d-1) g
        newB = min b s

-- Map a list of gameStates using sequential propagation of AB values
maxABEval :: ABMap
maxABEval a b = maxABFold abMM maxABEval a b
minABEval :: ABMap
minABEval a b = minABFold abMM minABEval a b

-- Map a list of gameStates using by mapping in parallel until the depthLimit
parABEval :: ABMap
parABEval a b = parMMVals (ybcwMM a b) (abMM a b)

-- Map a list by getting ab values from the first and then mapping in parallel
maxYBCWEval :: ABMap
maxYBCWEval a b = maxABFold ybcwMM parABEval a b
minYBCWEval :: ABMap
minYBCWEval a b = minABFold ybcwMM parABEval a b


interactivePlayer :: GameState -> IO GameState
interactivePlayer g@(PlayerTurn b) = do
    putStrLn "Choose Move (L, R, U, D)"
    m <- parsePlayerMove <$> getLine
    case m of
        Just m' -> do
            case lookup m' $ getPlayerMoves b of
                Just newState -> return newState
                Nothing -> getAnotherMove
        Nothing -> getAnotherMove
  where getAnotherMove = do putStrLn "Move invalid! Choose another move."
                            interactivePlayer g
interactivePlayer _ = error "It's not the Player's turn"
