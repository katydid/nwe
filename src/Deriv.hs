module Deriv where

import Grammar
import Simplify

type State a = [Expr a]
type Symbol a = [Expr a]

deriveCall :: (Eq a) => Refs a -> State a -> a -> (Symbol a, State a)
deriveCall refs e input = (e, map (flip evalIfExpr $ input) (concatMap (derivCall refs) e))

deriveReturn :: (Show a, Ord a) => Refs a -> (Symbol a, State a) -> a -> State a
deriveReturn refs (symbol, state) _ = derivReturns refs (symbol, (map (nullable refs) state))

deriveInternal :: (Show a, Ord a) => Refs a -> State a -> a -> State a
deriveInternal refs current input = let (symbol, callstate) = deriveCall refs current input
    in deriveReturn refs (symbol, callstate) input

type IfExpr a = (a, Expr a, Expr a)

evalIfExpr :: (Eq a) => IfExpr a -> a -> Expr a
evalIfExpr (v, thn, els) a = if v == a then thn else els 

derivCall :: Refs a -> Expr a -> [IfExpr a]
derivCall _ Empty = []
derivCall _ EmptySet = []
derivCall _ (Node v e) = [(v, e, EmptySet)]
derivCall refs (Concat l r) = if nullable refs l
    then derivCall refs l ++ derivCall refs r
    else derivCall refs l
derivCall refs (Or l r) = derivCall refs l ++ derivCall refs r
derivCall refs (And l r) = derivCall refs l ++ derivCall refs r
derivCall refs (Interleave l r) = derivCall refs l ++ derivCall refs r
derivCall refs (ZeroOrMore e) = derivCall refs e
derivCall refs (Reference name) = derivCall refs $ lookupRef refs name
derivCall refs (Not e) = derivCall refs e
derivCall refs (Contains e) = derivCall refs (Concat (Not EmptySet) (Concat e (Not EmptySet)))
derivCall refs (Optional e) = derivCall refs (Or e Empty)

derivReturns :: (Show a, Ord a) => Refs a -> ([Expr a], [Bool]) -> [Expr a]
derivReturns _ ([], []) = []
derivReturns refs (e:tailes, ns) =
    let (de, tailns) = derivReturn refs e ns
        se = simplify refs de
    in  se:derivReturns refs (tailes, tailns)

derivReturn :: Refs a -> Expr a -> [Bool] -> (Expr a, [Bool])
derivReturn _ Empty ns = (EmptySet, ns)
derivReturn _ EmptySet ns = (EmptySet, ns)
derivReturn _ (Node _ _) ns = if head ns 
    then (Empty, tail ns)
    else (EmptySet, tail ns)
derivReturn refs (Concat l r) ns = 
    if nullable refs l
    then    let (leftDeriv, leftTail) = derivReturn refs l ns
                (rightDeriv, rightTail) = derivReturn refs r leftTail
            in  (Or (Concat leftDeriv r) rightDeriv, rightTail)
    else    let (leftDeriv, leftTail) = derivReturn refs l ns
            in  (Concat leftDeriv r, leftTail)
derivReturn refs (Or l r) ns = 
    let (leftDeriv, leftTail) = derivReturn refs l ns
        (rightDeriv, rightTail) = derivReturn refs r leftTail
    in (Or leftDeriv rightDeriv, rightTail)
derivReturn refs (And l r) ns = 
    let (leftDeriv, leftTail) = derivReturn refs l ns
        (rightDeriv, rightTail) = derivReturn refs r leftTail
    in (And leftDeriv rightDeriv, rightTail)
derivReturn refs (Interleave l r) ns = 
    let (leftDeriv, leftTail) = derivReturn refs l ns
        (rightDeriv, rightTail) = derivReturn refs r leftTail
    in (Or (Interleave leftDeriv r) (Interleave rightDeriv l), rightTail)
derivReturn refs z@(ZeroOrMore e) ns = 
    let (derivexpr, tailns) = derivReturn refs e ns
    in  (Concat derivexpr z, tailns)
derivReturn refs (Reference name) ns = derivReturn refs (lookupRef refs name) ns
derivReturn refs (Not e) ns =
    let (derivp, tailns) = derivReturn refs e ns
    in  (Not derivp, tailns)
derivReturn refs (Contains e) ns = derivReturn refs (Concat (Not EmptySet) (Concat e (Not EmptySet))) ns
derivReturn refs (Optional e) ns = derivReturn refs (Or e Empty) ns