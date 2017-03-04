module Data.Algorithm.TSNE ( 
        tsne3D,
        TSNEOptions(..),
        TSNEOutput3D(..)
    ) where

import Control.Monad.Writer.Lazy (execWriter)
import Control.Monad.State.Lazy (evalStateT)

import Data.Algorithm.TSNE.Types
import Data.Algorithm.TSNE.Internals
import Data.Algorithm.TSNE.Utils

{- |

-}
tsne3D :: TSNEOptions -> TSNEInput -> IO [TSNEOutput3D]
tsne3D opts vs = do
    st <- initState $ length vs
    let ps = neighbourProbabilities opts vs
    return $ [output3D ps st] ++ (execWriter $ evalStateT (runTSNE opts vs ps) st)


