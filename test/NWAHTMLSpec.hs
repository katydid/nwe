module NWAHTMLSpec (nwahtmltests) where

import qualified Test.HUnit as HUnit
import NWA
import HTML
import Control.Monad (unless)

newtest :: String -> (State, [Transition String], [State]) -> String -> Bool -> HUnit.Test
newtest name (start, trans, finals) input want = HUnit.TestLabel name $ HUnit.TestCase $ case new start trans finals of
    (Left err) -> HUnit.assertFailure $ "given input: " ++ show (start, trans, finals) ++ " got error: " ++ show err
    (Right auto) -> case exec auto (parse input) of
        (Left err) -> HUnit.assertFailure $ "given input: " ++ show input ++ " got error: " ++ show err
        (Right got) -> unless (got == want) $ HUnit.assertFailure $ "want: " ++ show want ++ " got: " ++ show got

start = State 1
failed = State 2
opened = State 3

first = Symbol 1
open = Symbol 2

paren = (start, [
    (Call start "<a>" (first, opened)),
    (Return (Bottom, start) "</a>" failed),
    (Call opened "<a>" (open, opened)),
    (Return (open, opened) "</a>" opened),
    (Return (first, opened) "</a>" start),
    (Call failed "<a>" (open, failed)),
    (Return (Bottom, failed) "</a>" failed),
    (Return (first, failed) "</a>" failed),
    (Return (open, failed) "</a>" failed),
    (Internal start "a" start),
    (Internal failed "a" failed),
    (Internal opened "a" opened)], [start])  

nwahtmltests = HUnit.TestList [
    newtest "open close paren" paren "<a></a>" True,
    newtest "open paren" paren "<a>" False,
    newtest "close paren" paren "</a>" False,
    newtest "two open paren" paren "<a><a>" False,
    newtest "close paren" paren "</a>" False,
    newtest "two close paren" paren "</a></a>" False,
    newtest "open open close close paren" paren "<a><a></a></a>" True,
    newtest "open open open close close paren" paren "<a><a><a></a></a>" False,
    newtest "open close open close paren" paren "<a><a></a></a>" True,

    newtest "with letters open close paren" paren "<a>a</a>" True,
    newtest "with letters open paren" paren "a<a>a" False,
    newtest "with letters close paren" paren "</a>a" False,
    newtest "with letters two open paren" paren "<a><a>a" False,
    newtest "with letters close paren" paren "a</a>" False,
    newtest "with letters two close paren" paren "a</a>a</a>" False,
    newtest "with letters open open close close paren" paren "a<a>a<a>a</a>a</a>a" True,
    newtest "with letters open open open close close paren" paren "a<a>a<a>a<a>a</a>a</a>" False,
    newtest "with letters open close open close paren" paren "<a>a</a>a<a></a>" True,

    HUnit.TestCase (return ())]