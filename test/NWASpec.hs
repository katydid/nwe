module NWASpec where

import qualified Test.HUnit as HUnit
import NWA
import Control.Monad (unless)

newtest :: String -> NWA -> String -> Bool -> HUnit.Test
newtest name auto input want = HUnit.TestLabel name $ HUnit.TestCase $ case exec auto input of
    (Left err) -> HUnit.assertFailure $ "given input: " ++ input ++ " got error: " ++ show err
    (Right got) -> unless (got == want) $ HUnit.assertFailure $ "want: " ++ show want ++ " got: " ++ show got

nwatests = HUnit.TestList [
    newtest "no transitions or input" (new (State 1) [] [(State 1)]) "" True,

    HUnit.TestCase (return ())]