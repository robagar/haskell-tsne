module Data.Algorithm.TSNE.Types where

import Data.Default


data TSNEOptions = TSNEOptions {
    tsnePerplexity :: Int,
    tsneLearningRate :: Double
}

type TSNEInputValue =  [Double]
type TSNEInput =  [TSNEInputValue]

type Position3D = (Double,Double,Double)

data TSNEOutput3D = TSNEOutput3D {
    tsneIteration :: Int,
    tsneSolution3D :: [Position3D],
    tsneCost :: Double
} deriving (Show, Eq)

instance Default TSNEOptions where
    def = TSNEOptions 30 10

type Gain = Double
type Delta = Double
type Gradient = Double
type Entropy = Double

data TSNEState = TSNEState {
    stIteration :: Int,
    stSolution :: [[Double]],
    stGains :: [[Gain]],
    stDeltas :: [[Delta]]
}