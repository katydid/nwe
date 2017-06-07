module Simplify where

import qualified Data.Set as DataSet

import Grammar

simplify :: (Show a, Ord a) => Refs a -> Expr a -> Expr a
simplify refs expr =
    let simp = simplify' refs
    in case expr of
	Empty -> Empty
	EmptySet -> EmptySet
	(Internal v) -> Internal v
	(Call v e) -> Call v e
	(Return v) -> Return v
 	(Concat e1 e2) -> simplifyConcat (simp e1) (simp e2)
 	(Or e1 e2) -> simplifyOr refs (simp e1) (simp e2)
 	(And e1 e2) -> simplifyAnd refs (simp e1) (simp e2)
 	(ZeroOrMore e) -> simplifyZeroOrMore (simp e)
 	(Not e) -> simplifyNot (simp e)
 	(Optional e) -> simplifyOptional (simp e)
 	(Interleave e1 e2) -> simplifyInterleave (simp e1) (simp e2)
 	(Contains e) -> simplifyContains (simp e)
 	e@(Reference _) -> e
 	wtf -> error $ "unexpected expression: " ++ show wtf

simplify' :: (Show a, Ord a) => Refs a -> Expr a -> Expr a
simplify' refs e = checkRef refs $ simplify refs e

simplifyConcat :: Expr a -> Expr a -> Expr a
simplifyConcat EmptySet _ = EmptySet
simplifyConcat _ EmptySet = EmptySet
simplifyConcat (Concat e1 e2) e3 = 
	simplifyConcat e1 (Concat e2 e3)
simplifyConcat Empty e = e
simplifyConcat e Empty = e
simplifyConcat (Not EmptySet) (Concat e (Not EmptySet)) = Contains e
simplifyConcat e1 e2 = Concat e1 e2

simplifyOr :: (Ord a) => Refs a -> Expr a -> Expr a -> Expr a
simplifyOr _ EmptySet e = e
simplifyOr _ e EmptySet = e
simplifyOr _ (Not EmptySet) _ = (Not EmptySet)
simplifyOr _ _ (Not EmptySet) = (Not EmptySet)
simplifyOr refs Empty e = if nullable refs e then e else Or Empty e
simplifyOr refs e Empty = if nullable refs e then e else Or Empty e
simplifyOr _ e1 e2 = bin Or $ DataSet.toAscList $ setOfOrs e1 `DataSet.union` setOfOrs e2

bin :: (Expr a -> Expr a -> Expr a) -> [Expr a] -> Expr a
bin op [e] = e
bin op [e1,e2] = op e1 e2
bin op (e:es) = op e (bin op es)

setOfOrs :: (Ord a) => Expr a -> DataSet.Set (Expr a)
setOfOrs (Or e1 e2) = setOfOrs e1 `DataSet.union` setOfOrs e2
setOfOrs e = DataSet.singleton e

simplifyAnd :: (Ord a) => Refs a -> Expr a -> Expr a -> Expr a
simplifyAnd _ EmptySet _ = EmptySet
simplifyAnd _ _ EmptySet = EmptySet
simplifyAnd _ (Not EmptySet) e = e
simplifyAnd _ e (Not EmptySet) = e
simplifyAnd refs Empty e = if nullable refs e then Empty else EmptySet
simplifyAnd refs e Empty = if nullable refs e then Empty else EmptySet
simplifyAnd _ e1 e2 = bin And $ DataSet.toAscList $ setOfAnds e1 `DataSet.union` setOfAnds e2

setOfAnds :: (Ord a) => Expr a -> DataSet.Set (Expr a)
setOfAnds (And e1 e2) = setOfAnds e1 `DataSet.union` setOfAnds e2
setOfAnds e = DataSet.singleton e

simplifyZeroOrMore :: Expr a -> Expr a
simplifyZeroOrMore (ZeroOrMore e) = ZeroOrMore e
simplifyZeroOrMore e = ZeroOrMore e

simplifyNot :: Expr a -> Expr a
simplifyNot (Not e) = e
simplifyNot e = Not e

simplifyOptional :: Expr a -> Expr a
simplifyOptional Empty = Empty
simplifyOptional e = Optional e

simplifyInterleave :: (Ord a) => Expr a -> Expr a -> Expr a
simplifyInterleave EmptySet _ = EmptySet
simplifyInterleave _ EmptySet = EmptySet
simplifyInterleave Empty e = e
simplifyInterleave e Empty = e
simplifyInterleave (Not EmptySet) (Not EmptySet) = (Not EmptySet)
simplifyInterleave e1 e2 = bin Interleave $ DataSet.toAscList $ setOfInterleaves e1 `DataSet.union` setOfInterleaves e2

setOfInterleaves :: (Ord a) => Expr a -> DataSet.Set (Expr a)
setOfInterleaves (Interleave e1 e2) = setOfInterleaves e1 `DataSet.union` setOfInterleaves e2
setOfInterleaves e = DataSet.singleton e

simplifyContains :: Expr a -> Expr a
simplifyContains Empty = (Not EmptySet)
simplifyContains (Not EmptySet) = (Not EmptySet)
simplifyContains EmptySet = EmptySet
simplifyContains e = Contains e

checkRef :: (Eq a) => Refs a -> Expr a -> Expr a
checkRef refs e = case reverseLookupRef e refs of
	Nothing  	-> e
	(Just k) 	-> Reference k

