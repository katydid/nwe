module Main where

import qualified Test.HUnit as HUnit
import NWASpec

main :: IO ()
main = do { 
    counts <- HUnit.runTestTT nwatests;
    putStrLn $ show counts;
    return ()
}

