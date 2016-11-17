module Data.Algorithm.TSNE.Internals where

import Control.Applicative
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Exception (assert)
import Data.Default (def)
import Data.List(zipWith4)
import Data.Random.Normal (normalsIO')
--import Debug.Trace

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Utils


initState :: Int -> IO TSNEState
initState n = do
    s <- initSolution3D n
    return $ TSNEState 0 s (rr 1) (rr 0)
        where
            rr = repeat.repeat
            --f m = take 3 $ repeat $ take n $ repeat m 

initSolution3D :: Int -> IO [[Double]]
initSolution3D n = do
    let ns = normalsIO' (0, 1e-4)
    xs <- ns
    ys <- ns
    zs <- ns
    return $ take n <$> [xs,ys,zs]

runTSNE :: TSNEOptions -> TSNEInput -> [[Double]] -> (StateT TSNEState (Writer [TSNEOutput3D])) ()
runTSNE opts vs ps = forever $ do
    st <- get
    let st' = stepTSNE opts vs ps st
    tell $ [output3D ps st']
    put st'

stepTSNE :: TSNEOptions -> TSNEInput -> [[Double]] -> TSNEState -> TSNEState
stepTSNE opts vs ps st = TSNEState i' s'' g' d'
    where
        i = stIteration st
        s = stSolution st
        g = stGains st
        d = stDeltas st
        gr = gradients ps st
        i' = i + 1
        s' = recenter $ z (+) s d'
        g' = z3 newGain g d gr
        d' = z3 (newDelta (tsneLearningRate opts) i) g' d gr
        z = zipWith.zipWith
        z3 = zipWith3.zipWith3
        s'' = assert (length s' == length vs) s'

newGain :: Gain -> Delta -> Gradient -> Gain
newGain g d gr = max 0.01 g'
    where
        g' = if signum d == signum gr 
                then g * 0.8
                else g + 0.2  

newDelta :: Double -> Int -> Gain -> Delta -> Gradient -> Delta
newDelta e i g' d gr = (m * d) - (e * g' * gr)
    where
        m = if i < 250 then 0.5 else 0.8

gradients :: [[Double]] -> TSNEState -> [[Gradient]]
gradients pss st = zipWith4 (gradient i ss) ss pss qss qss'
    where
        ss = stSolution st
        i = stIteration st
        qss = qdist ss
        qss' = qdist' ss 
        gradient :: Int -> [[Double]] -> [Double] -> [Double] -> [Double] -> [Double] -> [Gradient]
        gradient i ss s ps qs qs' = (map sum) $ zipWith4 g ps qs qs' ss
            where
                g :: Double -> Double -> Double -> [Double] -> [Gradient]
                g p q q' t = zipWith (\x y -> m * (x - y)) s t
                    where
                        m = 4 * (k * p - q') * q
                        k = if i < 100 then 4 else 1


solution3D :: [[Double]] -> [Position3D]
solution3D (xs:ys:zs:_) = zip3 xs ys zs

output3D :: [[Double]] -> TSNEState -> TSNEOutput3D
output3D pss st = TSNEOutput3D i s c
    where
        i = stIteration st
        s = (solution3D . stSolution) st
        c = cost pss st

cost :: [[Double]] -> TSNEState -> Double
cost pss st = sumsum $ (zipWith.zipWith) c pss (qdist' (stSolution st))
    where
        c p q = -p * log q 

targetEntropy :: TSNEOptions -> Entropy
targetEntropy = log.realToFrac.tsnePerplexity

data Beta = Beta {
    betaValue :: Double,
    betaMin :: Double,
    betaMax :: Double
}

neighbourProbabilities :: TSNEOptions -> TSNEInput -> [[Double]]
neighbourProbabilities opts vs = symmetrize $ rawNeighbourProbabilities opts vs

rawNeighbourProbabilities :: TSNEOptions -> TSNEInput -> [[Double]]
rawNeighbourProbabilities opts vs = map np vs
    where 
        np a = aps (beta a) vs a
        beta a = betaValue $ binarySearchBeta opts vs a

        aps :: Double -> TSNEInput -> TSNEInputValue -> [Double]
        aps beta bs a = map pj' bs
            where
                psum = sum $ map pj bs
                pj b 
                    | a == b    = 0
                    | otherwise = exp $ -(distanceSquared a b) * beta 
                pj' b = pj b / psum

binarySearchBeta :: TSNEOptions -> TSNEInput -> TSNEInputValue -> Beta
binarySearchBeta opts vs = binarySearchBeta' opts vs 1e-4 0 (Beta 1 (-infinity) infinity)

binarySearchBeta' :: TSNEOptions -> TSNEInput -> Double -> Int -> Beta -> TSNEInputValue -> Beta
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

entropyForInputValue :: Double -> TSNEInput -> TSNEInputValue -> Entropy
entropyForInputValue beta bs a = sum $ map h bs
    where
        h b = if x > 1e-7 then -x * log x else 0
            where x = pj' b
        psum = sum $ map pj bs
        pj b 
            | a == b    = 0
            | otherwise = exp $ -(distanceSquared a b) * beta 
        pj' b = pj b / psum

