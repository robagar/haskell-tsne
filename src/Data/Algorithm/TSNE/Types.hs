module Data.Algorithm.TSNE.Types where

import Data.Default
import Data.Array.Repa
import Data.Vector as V
import Data.Vector.Unboxed as U


data TSNEOptions = TSNEOptions {
    tsnePerplexity :: Int,
    tsneLearningRate :: Double
}

type TSNEInputValue = [Double]
type TSNEInputValueU = U.Vector Double

type TSNEInput = [TSNEInputValue]
type TSNEInputVU = V.Vector TSNEInputValueU

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
type ProbabilityArray = Array U DIM2 Double

type Gain = Double
type Delta = Double
type Gradient = Double
type Entropy = Double

data TSNEState = TSNEState {
    stIteration :: Int,
    stSolution :: Array U DIM2 Double, -- 2D array of unboxed doubles
    stGains :: Array U DIM2 Double,
    stDeltas :: Array U DIM2 Double
} deriving (Show)