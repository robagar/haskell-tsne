module Data.Algorithm.TSNE.InternalsSpec (main, spec) where

import Test.Hspec
import Data.Algorithm.TSNE.Internals

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

u = undefined

spec :: Spec
spec = do
    describe "initSolution3D" $ do
        it "is right size" $
            initSolution3D 99 >>= (`shouldSatisfy` (\s -> length s == 3 && all (\xs -> length xs == 99) s))