module Data.Algorithm.TSNE.Checks where

isSquare :: Int -> [[a]] -> Bool
isSquare 0 [] = True
isSquare _ [] = False
isSquare n xss = length xss == n && all (\xs -> length xs == n) xss
