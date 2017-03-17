module Data.Algorithm.TSNE.Run3D where

import Control.Applicative
import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Random.Normal (normalsIO')
import Pipes

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Preparation
import Data.Algorithm.TSNE.Stepping


initState3D :: Int -> IO TSNEState
initState3D n = do
    s <- initSolution3D n
    return $ TSNEState 0 s (rr 1) (rr 0)
        where
            rr = (V.replicate 3).(U.replicate n)

initSolution3D :: Int -> IO Array2D
initSolution3D n = do
    let ns = normalsIO' (0, 1e-4)
    xs <- ns
    ys <- ns
    zs <- ns
    return $ V.fromList $ U.fromList . take n <$> [xs,ys,zs]

runTSNE3D :: TSNEOptions -> ProbabilityArray -> TSNEState -> Producer TSNEOutput3D IO ()
runTSNE3D opts ps st = do
    yield $ output3D ps st
    let st' = force $ stepTSNE opts ps st
    runTSNE3D opts ps st'

solution3D :: Array2D -> [Position3D]
solution3D ss = zip3 (f 0 ss) (f 1 ss) (f 2 ss)
    where
        f i ss = U.toList $ ss V.! i

output3D :: Array2D -> TSNEState -> TSNEOutput3D
output3D pss st = TSNEOutput3D i s c
    where
        i = stIteration st
        s = (solution3D . stSolution) st
        c = cost pss st

