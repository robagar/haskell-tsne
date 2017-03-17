{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Data.Algorithm.TSNE.Types where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Default
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U


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

type Array2D = V.Vector (U.Vector Double) -- 2D array of unboxed doubles

type Probability = Double
type ProbabilityArray = Array2D

type Gain = Double
type Delta = Double
type Gradient = Double
type GradientArray = Array2D
type Entropy = Double

data TSNEState = TSNEState {
    stIteration :: Int,
    stSolution :: Array2D,
    stGains :: Array2D,
    stDeltas :: Array2D
} deriving (Show, Generic, NFData)