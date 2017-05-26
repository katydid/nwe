module NWASpec where

import qualified Test.HUnit as HUnit
import NWA
import Control.Monad (unless)

newtest :: (Show a, Eq a, Ord a) => String -> (State, [Transition a], [State]) -> [a] -> Bool -> HUnit.Test
newtest name (start, trans, finals) input want = HUnit.TestLabel name $ HUnit.TestCase $ case new start trans finals of
    (Left err) -> HUnit.assertFailure $ "given input: " ++ show (start, trans, finals) ++ " got error: " ++ show err
    (Right auto) -> case exec auto input of
        (Left err) -> HUnit.assertFailure $ "given input: " ++ show input ++ " got error: " ++ show err
        (Right got) -> unless (got == want) $ HUnit.assertFailure $ "want: " ++ show want ++ " got: " ++ show got

start = State 1
failed = State 2
opened = State 3

first = Symbol 1
open = Symbol 2


paren = (start, [
    (Call start '(' (first, opened)),
    (Return (Bottom, start) ')' failed),
    (Call opened '(' (open, opened)),
    (Return (open, opened) ')' opened),
    (Return (first, opened) ')' start),
    (Call failed '(' (open, failed)),
    (Return (Bottom, failed) ')' failed),
    (Return (first, failed) ')' failed),
    (Return (open, failed) ')' failed),
    (Internal start 'a' start),
    (Internal failed 'a' failed),
    (Internal opened 'a' opened)], [start])
    

nwatests = HUnit.TestList [
    newtest "no transitions or input" ((State 1), [], [(State 1)]) "" True,

    newtest "open close paren" paren "()" True,
    newtest "open paren" paren "(" False,
    newtest "close paren" paren ")" False,
    newtest "two open paren" paren "((" False,
    newtest "close paren" paren ")" False,
    newtest "two close paren" paren "))" False,
    newtest "open open close close paren" paren "(())" True,
    newtest "open open open close close paren" paren "((())" False,
    newtest "open close open close paren" paren "()()" True,

    newtest "with letters open close paren" paren "(a)" True,
    newtest "with letters open paren" paren "a(a" False,
    newtest "with letters close paren" paren ")aa" False,
    newtest "with letters two open paren" paren "((a" False,
    newtest "with letters close paren" paren "a)" False,
    newtest "with letters two close paren" paren "a)a)" False,
    newtest "with letters open open close close paren" paren "aa(a(a)aaa)aa" True,
    newtest "with letters open open open close close paren" paren "a(a(a(aaaaa)aaaa)" False,
    newtest "with letters open close open close paren" paren "(a)aaaaa()" True,

    HUnit.TestCase (return ())]