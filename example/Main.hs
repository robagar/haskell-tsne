module Main where

import System.Environment(getArgs)
import System.Exit
import Control.Monad(when)
import Data.Default(def)
import Data.Algorithm.TSNE


main :: IO ()
main = do
    putStrLn "Haskell tSNE example"

    args <- getArgs
    let argc = length args
    when (argc < 1 || argc > 2) $ do
        putStrLn "Usage haskell_tsne_example {input file name} [{num input values}]"
        exitFailure

    let inputFileName = head args
    inputData <- readDataFile inputFileName

    let n = length inputData
    putStrLn $ "total input: " ++ show n

    let n' = if (argc < 2) then n else (read (args !! 1)) 

    putStrLn $ "using: " ++ show n'

    solutions <- tsne3D def $ take n' inputData
    outputSolutions solutions

readDataFile :: FilePath -> IO [[Double]]
readDataFile f = do
    d <- readFile f
    return $ map read (lines d)

outputSolutions :: [TSNEOutput3D] -> IO()
outputSolutions (s:ss) = do
    putStrLn $ "iteration: " ++ (show.tsneIteration) s
    putStrLn $ "cost: " ++ (show.tsneCost) s
    outputSolutions ss

