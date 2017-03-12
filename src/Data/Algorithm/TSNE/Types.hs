module Data.Algorithm.TSNE.Types where

import Data.Default
import qualified Data.Array.Repa as R


data TSNEOptions = TSNEOptions {
    tsnePerplexity :: Int,
    tsneLearningRate :: Double
}

type TSNEInputValue =  [Double]
type TSNEInput =  [TSNEInputValue]

type Position3D = (Double,Double,Double)

data TSNEOutput3D = TSNEOutput3D {
    tsneIteration3D :: Int,
    tsneSolution3D :: [Position3D],
    tsneCost3D :: Double
} deriving (Show, Eq)

type Position2D = (Double,Double)

data TSNEOutput2D = TSNEOutput2D {
    tsneIteration2D :: Int,
    tsneSolution2D :: [Position2D],
    tsneCost2D :: Double
} deriving (Show, Eq)

instance Default TSNEOptions where
    def = TSNEOptions 30 10

type Probability = Double
type Gain = Double
type Delta = Double
type Gradient = Double
type Entropy = Double

data TSNEState = TSNEState {
    stIteration :: Int,
    stSolution :: R.Array R.U R.DIM2 Double, -- 2D array of unboxed doubles
    stGains :: R.Array R.U R.DIM2 Double,
    stDeltas :: R.Array R.U R.DIM2 Double
} deriving (Show)