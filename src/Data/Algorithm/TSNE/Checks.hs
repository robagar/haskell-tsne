module Data.Algorithm.TSNE.Checks where

import Data.Algorithm.TSNE.Types

isSquare :: Int -> [[a]] -> Bool
isSquare 0 [] = True
isSquare _ [] = False
isSquare n xss = length xss == n && all (\xs -> length xs == n) xss

has2DShape :: (Int, Int) -> [[a]] -> Bool
has2DShape (w,h) xss = length xss == h && all (\xs -> length xs == w) xss

shape2D :: [[a]] -> (Int, Int)
shape2D [] = (undefined, 0)
shape2D xss = (length (head xss), length xss)

isRectangular :: [[a]] -> Bool
isRectangular xss = has2DShape (shape2D xss) xss

