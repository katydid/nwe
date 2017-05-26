module Main where

import qualified Test.HUnit as HUnit
import NWASpec
import HTMLSpec

main :: IO ()
main = do { 
    nwacounts <- HUnit.runTestTT nwatests;
    putStrLn $ show nwacounts;

    htmlcounts <- HUnit.runTestTT htmltests;
    putStrLn $ show htmlcounts;

    return ()
}

