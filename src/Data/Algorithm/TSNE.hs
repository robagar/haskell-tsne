module Data.Algorithm.TSNE ( 
        tsne,
        TSNEOptions(..), def,
        TSNEOutput3D(..)
    ) where

import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Data.Default
import Data.List(foldl')
import Data.Random.Normal (normalsIO')
import Debug.Trace

data TSNEOptions = TSNEOptions {
    tsnePerplexity :: Int,
    tsneLearningRate :: Float
}

type TSNEInputValue =  [Float]
type TSNEInput =  [TSNEInputValue]

type Position3D = (Float,Float,Float)

data TSNEOutput3D = TSNEOutput3D {
    tsneIteration :: Int,
    tsneSolution3D :: [Position3D],
    tsneCost :: Float
}

instance Default TSNEOptions where
    def = TSNEOptions 30 10

data TSNEState = TSNEState {
    stIteration :: Int,
    stSolution :: [[Float]]
}

tsne :: TSNEOptions -> TSNEInput -> IO [TSNEOutput3D]
tsne opts vs = do
    st <- initState $ length vs
    return $ [output3D st] ++ (execWriter $ evalStateT (runTSNE opts vs) st)

initState :: Int -> IO TSNEState
initState n = do
    s <- initSolution3D n
    return $ TSNEState 0 s

runTSNE :: TSNEOptions -> TSNEInput -> (StateT TSNEState (Writer [TSNEOutput3D])) ()
runTSNE opts vs = forever $ do
    st <- get
    let st' = stepTSNE opts vs st
    when (shouldOutput st st') $ do 
        tell $ [output3D st']
    put st'

stepTSNE :: TSNEOptions -> TSNEInput -> TSNEState -> TSNEState
stepTSNE opts vs st = TSNEState i s
    where
        i = stIteration st + 1
        s = undefined

initSolution3D :: Int -> IO [[Float]]
initSolution3D n = do
    let ns = normalsIO' (0, 1e-4)
    xs <- ns
    ys <- ns
    zs <- ns
    return $ map (take n) [xs,ys,zs]

solution3D :: [[Float]] -> [Position3D]
solution3D (xs:ys:zs:_) = zip3 xs ys zs

shouldOutput :: TSNEState -> TSNEState -> Bool
shouldOutput _ _ = True -- TODO

output3D :: TSNEState -> TSNEOutput3D
output3D st = TSNEOutput3D i s c
    where
        i = stIteration st
        s = (solution3D . stSolution) st
        c = cost st

cost :: TSNEState -> Float
cost st = undefined

infinity :: Float
infinity = read "Infinity"
 
targetEntropy :: Float -> Float
targetEntropy = log

distanceSquared :: [Float] -> [Float] -> Float
distanceSquared as bs = foldl' d 0 (zip as bs)
    where d t (a,b) = t + (a-b) * (a-b)

neighbourProbability :: (Int, [TSNEInputValue]) -> (Int, [TSNEInputValue]) -> Float
neighbourProbability (i,a) (j,b) 
    | i == j    = 0
    | otherwise = undefined

