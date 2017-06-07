module Deriv where

import Grammar
import Simplify
import qualified Stack as Stack

deriv :: (Eq a, Show a, Ord a) => Refs a -> (Stack.Stack (Symbol a), State a) -> a -> (Stack.Stack (Symbol a), State a)
deriv refs (stack, current) input
    | isCall input = 
        let (symbol, next) = deriveCall refs current input 
        in (Stack.push (Stack.Symbol symbol) stack, next)
    | isReturn input = case Stack.peek stack of
        Stack.Bottom -> undefined -- TODO
        (Stack.Symbol symbol) ->    let next = deriveReturn refs (symbol, current) input 
                                    in (Stack.pop stack, next)
    | otherwise = 
        (stack, deriveInternal refs current input)
    where isCall = newIsCall refs current
          isReturn = newIsReturn refs current

newIsCall :: Refs a -> [Expr a] -> (a -> Bool)
newIsCall = undefined -- TODO

newIsReturn :: Refs a -> [Expr a] -> (a -> Bool)
newIsReturn = undefined -- TODO

type State a = [Expr a]
type Symbol a = [Expr a]

deriveCall :: (Eq a) => Refs a -> State a -> a -> (Symbol a, State a)
deriveCall refs e input = (e, map (flip evalIfExpr $ input) (concatMap (derivCall refs) e))

deriveReturn :: (Show a, Ord a) => Refs a -> (Symbol a, State a) -> a -> State a
deriveReturn refs (symbol, state) input = derivReturns refs input (symbol, (map (nullable refs) state))

deriveInternal :: (Show a, Ord a) => Refs a -> State a -> a -> State a
deriveInternal refs current input = let (symbol, callstate) = deriveCall refs current input
    in deriveReturn refs (symbol, callstate) input

type IfExpr a = (a, Expr a, Expr a)

evalIfExpr :: (Eq a) => IfExpr a -> a -> Expr a
evalIfExpr (v, thn, els) a = if v == a then thn else els 

derivCall :: Refs a -> Expr a -> [IfExpr a]
derivCall _ Empty = []
derivCall _ EmptySet = []
derivCall _ (Internal _) = undefined -- TODO
derivCall _ (Call v e) = [(v, e, EmptySet)]
derivCall _ (Return _) = undefined -- TODO
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

derivReturns :: (Show a, Ord a) => Refs a -> a -> ([Expr a], [Bool]) -> [Expr a]
derivReturns _ _ ([], []) = []
derivReturns refs input (e:tailes, ns) =
    let (de, tailns) = derivReturn refs input e ns
        se = simplify refs de
    in  se:derivReturns refs input (tailes, tailns)

derivReturn :: (Eq a) => Refs a -> a -> Expr a -> [Bool] -> (Expr a, [Bool])
derivReturn _ _ Empty ns = (EmptySet, ns)
derivReturn _ _ EmptySet ns = (EmptySet, ns)
derivReturn _ _ (Internal _) _ = undefined -- TODO
derivReturn _ _ (Call _ _) _ = undefined -- TODO
derivReturn _ input (Return v) ns = if head ns 
    then (evalIfExpr (input, Empty, EmptySet) v, tail ns)
    else (EmptySet, tail ns)
derivReturn refs input (Concat l r) ns = 
    if nullable refs l
    then    let (leftDeriv, leftTail) = derivReturn refs input l ns
                (rightDeriv, rightTail) = derivReturn refs input r leftTail
            in  (Or (Concat leftDeriv r) rightDeriv, rightTail)
    else    let (leftDeriv, leftTail) = derivReturn refs input l ns
            in  (Concat leftDeriv r, leftTail)
derivReturn refs input (Or l r) ns = 
    let (leftDeriv, leftTail) = derivReturn refs input l ns
        (rightDeriv, rightTail) = derivReturn refs input r leftTail
    in (Or leftDeriv rightDeriv, rightTail)
derivReturn refs input (And l r) ns = 
    let (leftDeriv, leftTail) = derivReturn refs input l ns
        (rightDeriv, rightTail) = derivReturn refs input r leftTail
    in (And leftDeriv rightDeriv, rightTail)
derivReturn refs input (Interleave l r) ns = 
    let (leftDeriv, leftTail) = derivReturn refs input l ns
        (rightDeriv, rightTail) = derivReturn refs input r leftTail
    in (Or (Interleave leftDeriv r) (Interleave rightDeriv l), rightTail)
derivReturn refs input z@(ZeroOrMore e) ns = 
    let (derivexpr, tailns) = derivReturn refs input e ns
    in  (Concat derivexpr z, tailns)
derivReturn refs input (Reference name) ns = derivReturn refs input (lookupRef refs name) ns
derivReturn refs input (Not e) ns =
    let (derivp, tailns) = derivReturn refs input e ns
    in  (Not derivp, tailns)
derivReturn refs input (Contains e) ns = derivReturn refs input (Concat (Not EmptySet) (Concat e (Not EmptySet))) ns
derivReturn refs input (Optional e) ns = derivReturn refs input (Or e Empty) ns