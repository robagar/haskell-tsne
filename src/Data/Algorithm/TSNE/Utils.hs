module Data.Algorithm.TSNE.Utils where

import Data.Vector.Unboxed as U
import Data.List(foldr, transpose)

infinity :: Double
infinity = read "Infinity"
 
distanceSquared :: U.Vector Double -> U.Vector Double -> Double
distanceSquared as bs = U.foldr d 0 (U.zip as bs)
    where d (a,b) t  = t + (a-b) * (a-b)

symmetrize :: [[Double]] -> [[Double]]
symmetrize m = undefined
--symmetrize m = (zipWith.zipWith) f m (transpose m)
--    where 
--        f :: Double -> Double -> Double
--        f x y = max a 1e-100
--            where a = (x + y) / (2 * (realToFrac.length) m) 

recenter :: [[Double]] -> [[Double]]
recenter ss = undefined
--recenter ss = map r ss
--    where 
--        r s = subtract (mean s) <$> s
--        mean s = sum s / (realToFrac.length) s
         
qdist :: [[Double]] -> [[Double]]
qdist ss = undefined
--qdist ss = symmetricalMatrixFromTopRight $ qd (transpose ss)
--    where
--        qd [] = []
--        qd ps = [qr ps] ++ qd (tail ps)
--        qr :: [[Double]] -> [Double]
--        qr ps =  [0::Double] ++ (q (head ps) <$> tail ps)
--        q as bs = 1 / (1 + s)
--            where
--                s = sum $ zipWith f as bs
--                f a b = (a-b) * (a-b) 

qdist' :: [[Double]] -> [[Double]]
qdist' ss = undefined
--qdist' ss = (map.map) f qd
--    where
--        qd = qdist ss
--        f :: Double -> Double 
--        f q = max (q / sumsum qd) 1e-100
 
sumsum :: [[Double]] -> Double
sumsum m = undefined
--sumsum m = sum $ sum <$> m 

reprep :: a -> [[a]]
reprep = undefined
--reprep = repeat.repeat

symmetricalMatrixFromTopRight :: [[a]] -> [[a]]
symmetricalMatrixFromTopRight tr = undefined
--symmetricalMatrixFromTopRight tr = zipWith (++) bl tr
--    where
--        bl = zipWith take [0..] (transpose m)
--        m = zipWith (++) ebl tr
--        ebl = zipWith take [0..] (reprep undefined)



