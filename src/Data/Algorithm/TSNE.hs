module Data.Algorithm.TSNE ( 
        tsne,
        TSNEOptions(..), def,
        TSNEOutput3D(..)
    ) where

import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Data.Default
import Data.List(foldr, transpose)
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

type Gain = Float
type Delta = Float
type Gradient = Float

data TSNEState = TSNEState {
    stIteration :: Int,
    stSolution :: [[Float]],
    stGains :: [[Gain]],
    stDeltas :: [[Delta]]
}

tsne :: TSNEOptions -> TSNEInput -> IO [TSNEOutput3D]
tsne opts vs = do
    st <- initState $ length vs
    return $ [output3D st] ++ (execWriter $ evalStateT (runTSNE opts vs) st)

initState :: Int -> IO TSNEState
initState n = do
    s <- initSolution3D n
    return $ TSNEState 0 s (rr 1) (rr 0)
        where
            rr = repeat.repeat
            --f m = take 3 $ repeat $ take n $ repeat m 

initSolution3D :: Int -> IO [[Float]]
initSolution3D n = do
    let ns = normalsIO' (0, 1e-4)
    xs <- ns
    ys <- ns
    zs <- ns
    return $ map (take n) [xs,ys,zs]

runTSNE :: TSNEOptions -> TSNEInput -> (StateT TSNEState (Writer [TSNEOutput3D])) ()
runTSNE opts vs = forever $ do
    st <- get
    let st' = stepTSNE opts vs st
    tell $ [output3D st']
    put st'

stepTSNE :: TSNEOptions -> TSNEInput -> TSNEState -> TSNEState
stepTSNE opts vs st = TSNEState i' s' g' d'
    where
        i = stIteration st
        s = stSolution st
        g = stGains st
        d = stDeltas st
        gr = gradients st
        i' = i + 1
        s' = recenter $ z (+) s d'
        g' = z3 newGain g d gr
        d' = z3 (newDelta (tsneLearningRate opts) i) g' d gr
        z = zipWith.zipWith
        z3 = zipWith3.zipWith3

newGain :: Gain -> Delta -> Gradient -> Gain
newGain g d gr = max 0.01 g'
    where
        g' = if signum d == signum gr 
                then g * 0.8
                else g + 0.2  

newDelta :: Float -> Int -> Gain -> Delta -> Gradient -> Delta
newDelta e i g' d gr = (m * d) - (e * g' * gr)
    where
        m = if i < 250 then 0.5 else 0.8

gradients :: TSNEState -> [[Gradient]]
gradients st = undefined 

solution3D :: [[Float]] -> [Position3D]
solution3D (xs:ys:zs:_) = zip3 xs ys zs

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
 
targetEntropy :: TSNEOptions -> Float
targetEntropy = log.realToFrac.tsnePerplexity

distanceSquared :: [Float] -> [Float] -> Float
distanceSquared as bs = foldr d 0 (zip as bs)
    where d (a,b) t  = t + (a-b) * (a-b)

data Beta = Beta {
    betaValue :: Float,
    betaMin :: Float,
    betaMax :: Float
}

neighbourProbabilities :: TSNEOptions -> TSNEInput -> [[Float]]
neighbourProbabilities opts vs = symmetrize $ rawNeighbourProbabilities opts vs

symmetrize :: [[Float]] -> [[Float]]
symmetrize m = (zipWith.zipWith) f m (transpose m)
    where 
        f :: Float -> Float -> Float
        f x y = max a 1e-100
            where a = (x + y) / (2 * (realToFrac.length) m) 

rawNeighbourProbabilities :: TSNEOptions -> TSNEInput -> [[Float]]
rawNeighbourProbabilities opts vs = map np vs
    where 
        np a = aps (beta a) vs a
        beta a = betaValue $ binarySearchBeta opts vs a

        aps :: Float -> TSNEInput -> TSNEInputValue -> [Float]
        aps beta bs a = map pj' bs
            where
                psum = sum $ map pj bs
                pj b 
                    | a == b    = 0
                    | otherwise = exp $ -(distanceSquared a b) * beta 
                pj' b = pj b / psum

binarySearchBeta :: TSNEOptions -> TSNEInput -> TSNEInputValue -> Beta
binarySearchBeta opts vs = binarySearchBeta' opts vs 1e-4 0 (Beta 1 (-infinity) infinity)

binarySearchBeta' :: TSNEOptions -> TSNEInput -> Float -> Int -> Beta -> TSNEInputValue -> Beta
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

entropyForInputValue :: Float -> TSNEInput -> TSNEInputValue -> Float
entropyForInputValue beta bs a = sum $ map h bs
    where
        h b = if x > 1e-7 then -x * log x else 0
            where x = pj' b
        psum = sum $ map pj bs
        pj b 
            | a == b    = 0
            | otherwise = exp $ -(distanceSquared a b) * beta 
        pj' b = pj b / psum

recenter :: [[Float]] -> [[Float]]
recenter vs = map r vs
    where 
        r v = map (subtract (mean v)) v
        mean v = sum v / (realToFrac.length) v
         



--neighbourProbability :: (Int, TSNEInputValue) -> (Int, TSNEInputValue) -> Float
--neighbourProbability (i,a) (j,b) 
--    | i == j    = 0
--    | otherwise = undefined

--entropy :: Float -> (Int, TSNEInputValue) -> [(Int, TSNEInputValue)] -> Float
--entropy beta (i,a) ibs = undefined
--    where
--        dh (j,b) = if pjb > 1e-7 then -pjb * log pjb else 0
--            where pjb = pj (j,b)  
--        psum = sum $ map pj ibs
--        pj (j,b) 
--            | i == j    = 0
--            | otherwise = exp $ -(distanceSquared a b) * beta 
--        pj' (j,b) = pj (j,b) / psum

--indexedInput :: TSNEInput -> [(Int, TSNEInputValue)]
--indexedInput = zip [0..]  
