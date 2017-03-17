module Data.Algorithm.TSNE.Preparation where

import Data.Vector as V
import Data.Vector.Unboxed as U


import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils


targetEntropy :: TSNEOptions -> Entropy
targetEntropy = log.realToFrac.tsnePerplexity

type BetaValue = Double
data Beta = Beta {
    betaValue :: BetaValue,
    betaMin :: BetaValue,
    betaMax :: BetaValue
}

neighbourProbabilities :: TSNEOptions -> TSNEInput -> ProbabilityArray
neighbourProbabilities opts vs = symmetrize $ rawNeighbourProbabilities opts (fromListVU vs)

rawNeighbourProbabilities :: TSNEOptions -> TSNEInputVU -> ProbabilityArray
rawNeighbourProbabilities opts vs = V.map np vs
    where 
        np a = aps (beta a) vs a
        beta a = betaValue $ binarySearchBeta opts vs a

        aps :: Double -> TSNEInputVU -> TSNEInputValueU -> U.Vector Probability
        aps beta bs a = convert $ V.map pj' bs
            where
                psum = V.sum $ V.map pj bs
                pj b 
                    | a == b    = 0
                    | otherwise = exp $ -(distanceSquared a b) * beta 
                pj' b = pj b / psum

binarySearchBeta :: TSNEOptions -> TSNEInputVU -> TSNEInputValueU -> Beta
binarySearchBeta opts vs = binarySearchBeta' opts vs 1e-4 0 (Beta 1 (-infinity) infinity)

type Tolerance = Double

binarySearchBeta' :: TSNEOptions -> TSNEInputVU -> Tolerance -> Int -> Beta -> TSNEInputValueU -> Beta
binarySearchBeta' opts bs tol i beta a
    | i == 50            = beta
    | abs (e - t) < tol  = beta
    | e > t              = r $ incPrecision beta
    | otherwise          = r $ decPrecision beta 
        where
            t = targetEntropy opts
            e = entropyForInputValue (betaValue beta) bs a
            incPrecision (Beta b _ bmax) 
                | bmax == infinity = Beta (b * 2) b bmax
                | otherwise        = Beta ((b + bmax) / 2) b bmax
            decPrecision (Beta b bmin _) 
                | bmin == -infinity = Beta (b / 2) bmin b
                | otherwise         = Beta ((b + bmin) / 2) bmin b
            r beta' = binarySearchBeta' opts bs tol (i+1) beta' a 

entropyForInputValue :: BetaValue -> TSNEInputVU -> TSNEInputValueU -> Entropy
entropyForInputValue beta bs a = V.sum $ V.map h bs
    where
        h b = if x > 1e-7 then -x * log x else 0
            where x = pj' b
        psum = V.sum $ V.map pj bs
        pj b 
            | a == b    = 0
            | otherwise = exp $ -(distanceSquared a b) * beta 
        pj' b = pj b / psum


