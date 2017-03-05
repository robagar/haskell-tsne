module Data.Algorithm.TSNE ( 
        tsne3D,
        TSNEOptions(..),
        TSNEOutput3D(..)
    ) where

import Pipes

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Internals
import Data.Algorithm.TSNE.Utils

{- |

-}
tsne3D :: TSNEOptions -> TSNEInput -> Producer TSNEOutput3D IO ()
tsne3D opts vs = do
    st <- liftIO $ initState $ length vs
    let ps = neighbourProbabilities opts vs
    runTSNE opts vs ps st



