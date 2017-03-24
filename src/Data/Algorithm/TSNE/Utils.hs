module Data.Algorithm.TSNE.Utils where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List(foldr, transpose)
import Data.Algorithm.TSNE.Types

--import Debug.Trace

infinity :: Double
infinity = read "Infinity"
 
distanceSquared :: U.Vector Double -> U.Vector Double -> Double
distanceSquared as bs = U.foldr d 0 (U.zip as bs)
    where d (a,b) t  = t + (a-b) * (a-b)

symmetrize :: Array2D -> Array2D
symmetrize m = (V.zipWith . U.zipWith) f m (transposeVU m)
    where 
        f :: Double -> Double -> Double
        f x y = max a 1e-100
            where a = (x + y) / (2 * (realToFrac . V.length) m) 

recenter :: Array2D -> Array2D
recenter ss = V.map r ss
    where 
        r s = U.map (subtract (mean s)) s
        mean s = U.sum s / (realToFrac.U.length) s
         
qdist :: Array2D -> Array2D
qdist ss = symmetricalMatrixFromTopRight $ qd (transposeVU ss)
    where
        qd ps 
            | V.null ps = V.empty
            | otherwise = V.singleton(qr ps) V.++ qd (V.tail ps)
                            where
                                qr :: Array2D -> U.Vector Double
                                qr ps = U.singleton 0 U.++ U.convert (q (V.head ps) <$> V.tail ps)
                                q as bs = 1 / (1 + s)
                                    where
                                        s = U.sum $ U.zipWith f as bs
                                        f a b = (a-b) * (a-b) 

qdist' :: Array2D -> Array2D
qdist' ss = (V.map . U.map) f qd
    where
        qd = qdist ss
        f :: Double -> Double 
        f q = max (q / sumsum qd) 1e-100
 
sumsum :: Array2D -> Double
sumsum m = V.sum $ U.sum <$> m 

reprep :: Int -> Double -> Array2D
reprep n a = V.replicate n (U.replicate n a)

symmetricalMatrixFromTopRight :: Array2D -> Array2D
symmetricalMatrixFromTopRight tr = V.zipWith (U.++) bl tr
    where
        bl = V.zipWith U.take (V.fromList [0..n]) (transposeVU m)
        m = V.zipWith (U.++) ebl tr
        ebl = V.zipWith U.take (V.fromList [0..n]) (reprep n 0)
        n = V.length tr

transposeVU :: Array2D -> Array2D
transposeVU m = if ((U.null . V.head) m) 
                    then V.empty
                    else V.convert (V.map U.head m) `V.cons` transposeVU (V.map U.tail m) 

zipWithVU :: (Double -> Double -> Double) -> Array2D -> Array2D -> Array2D
zipWithVU = V.zipWith . U.zipWith

fromListVU :: [[Double]] -> Array2D
fromListVU m = V.fromList $ map U.fromList m
