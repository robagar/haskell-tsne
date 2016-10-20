module Data.Algorithm.TSNE ( 
        tsne
    ) where

import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Data.List(foldl')
import Data.Random.Normal (normalsIO)

type Vec3 = (Float,Float,Float)

data TSNEState = TSNEState {
    stPositions :: [Vec3]
}

tsne :: Float -> Float -> [[Float]] -> IO [[Vec3]]
tsne p e vs = do
    st <- initState vs
    return $ execWriter $ evalStateT (runTSNE p e) st

initState :: [[Float]] -> IO TSNEState
initState vs = do
    xs <- normalsIO
    ys <- normalsIO
    zs <- normalsIO
    return $ TSNEState (take n (zip3 xs ys zs)) 
        where n = length vs

runTSNE :: Float -> Float -> (StateT TSNEState (Writer [[Vec3]])) ()
runTSNE p e = forever $ do
    st <- get
    tell $ [stPositions st]


distanceSquared :: [Float] -> [Float] -> Float
distanceSquared as bs = foldl' d 0 (zip as bs)
    where d t (a,b) = t + (a-b) * (a-b)
