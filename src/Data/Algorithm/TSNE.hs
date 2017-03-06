module Data.Algorithm.TSNE ( 
        tsne3D,
        forTsne3D,
        TSNEOptions(..),
        TSNEOutput3D(..)
    ) where

import Pipes

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Internals
import Data.Algorithm.TSNE.Utils

-- | Generates an infinite stream of 3D tSNE iterations.
tsne3D :: TSNEOptions -> TSNEInput -> Producer TSNEOutput3D IO ()
tsne3D opts input = do
    st <- liftIO $ initState $ length input
    runTSNE opts input ps st
        where ps = neighbourProbabilities opts input

-- | Executes an IO action for each iteration of the 3D tSNE algorithm.
forTsne3D :: (TSNEOutput3D -> IO ()) -> TSNEOptions -> TSNEInput -> IO ()
forTsne3D action opts input = do
    runEffect $ for (tsne3D opts input) $ \o -> do
        lift $ action o    

