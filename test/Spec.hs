module Main where

import qualified Test.HUnit as HUnit
import HTMLSpec
import NWASpec
import NWAHTMLSpec

main :: IO ()
main = do { 

    putStrLn "\nTesting HTML Parsing";
    htmlcounts <- HUnit.runTestTT htmltests;
    putStrLn $ show htmlcounts;

    putStrLn "\nTesting Nested Word Automaton";
    nwacounts <- HUnit.runTestTT nwatests;
    putStrLn $ show nwacounts;

    putStrLn "\nTesting Nested Word Automaton on HTML";
    nwahtmlcounts <- HUnit.runTestTT nwahtmltests;
    putStrLn $ show nwahtmlcounts;

    return ()
}

