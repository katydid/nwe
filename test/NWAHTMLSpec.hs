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

alpha = Symbol 1
beta = Symbol 2
gamma = Symbol 3

-- example from VPAs vs Tree Automata in the slides of Language Automata and Logic for Finite Trees - Oliver Gauwin
exampleOliver = (State 0, [
    (Call (State 0) "<a>" (alpha, State 1)),
    (Call (State 1) "<a>" (beta, State 1)),
    (Call (State 1) "<b>" (beta, State 4)),
    (Return (gamma, State 4) "</b>" (State 3)),
    (Return (beta, State 4) "</b>" (State 3)),
    (Return (beta, State 3) "</a>" (State 2)),
    (Return (alpha, State 3) "</a>" (State 5)),
    (Call (State 2) "<b>" (gamma, State 4))], [State 5])

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

    newtest "example from Oliver Gauwin" exampleOliver "<a><a><b></b></a><b></b></a>" True,

    HUnit.TestCase (return ())]