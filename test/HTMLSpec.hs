module HTMLSpec (htmltests) where

import qualified Test.HUnit as HUnit
import HTML
import Control.Monad (unless)

newtest :: String -> [String] -> HUnit.Test
newtest input want = HUnit.TestLabel input $ HUnit.TestCase $ 
    let got = parse input in unless (got == want) $ HUnit.assertFailure $ "want: " ++ show want ++ " got: " ++ show got

htmltests = HUnit.TestList [
    newtest "<a></a>" ["<a>", "</a>"],
    newtest "<a>a</a>" ["<a>", "a", "</a>"],
    newtest "<a><b>a</c>" ["<a>", "<b>", "a", "</c>"],
    newtest "<A a=b></A>" ["<A>", "a=b", "</A>"],
    newtest "<a/>" ["<a>", "</a>"],
    newtest "<a>a\na</a>" ["<a>", "a\na", "</a>"],

    HUnit.TestCase (return ())]