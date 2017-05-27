module Grammar where

import qualified Data.Map.Strict as DataMap
import qualified Data.Set as DataSet

data Expr a = Empty
    | EmptySet
    | Node a (Expr a)
    | Or (Expr a) (Expr a)
    | And (Expr a) (Expr a)
    | Not (Expr a)
    | Concat (Expr a) (Expr a)
    | Interleave (Expr a) (Expr a)
    | ZeroOrMore (Expr a)
    | Optional (Expr a)
    | Contains (Expr a)
    | Reference String
    deriving (Eq, Ord, Show)

-- instance Eq a => Eq (Expr a) where
--     Empty == Empty = True
--     EmptySet == EmptySet = True
--     Node a b == Node a' b' = a == a' && b == b'
--     Or a b == Or a' b' = a == a' && b == b'
--     And a b == And a' b' = a == a' && b == b'
--     Not a == Not a' = a == a'
--     Concat a b == Concat a' b' = a == a' && b == b'
--     Interleave a b == Interleave a' b' = a == a' && b == b'
--     ZeroOrMore a == ZeroOrMore a' = a == a'
--     Optional a == Optional a' = a == a'
--     Contains a == Contains a' = a == a'
--     Reference a == Reference a' = a == a'
--     _ == _ = False

nullable :: Refs a -> Expr a -> Bool
nullable _ Empty = True
nullable _ EmptySet = False
nullable _ (Node _ _) = False
nullable refs (Or l r) = nullable refs l || nullable refs r
nullable refs (And l r) = nullable refs l && nullable refs r
nullable refs (Not p) = not $ nullable refs p
nullable refs (Concat l r) = nullable refs l && nullable refs r
nullable refs (Interleave l r) = nullable refs l && nullable refs r
nullable _ (ZeroOrMore _) = True
nullable _ (Optional _) = True
nullable refs (Contains p) = nullable refs p
nullable refs (Reference name) = nullable refs $ lookupRef refs name

-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all Exprs are unescapable.
unescapable :: Expr a -> Bool
unescapable EmptySet = True
unescapable (Not EmptySet) = True
unescapable _ = False

newtype Refs a = Refs (DataMap.Map String (Expr a))
    deriving (Show, Eq)

lookupRef :: Refs a -> String -> Expr a
lookupRef (Refs m) name = m DataMap.! name

reverseLookupRef :: (Eq a) => Expr a -> Refs a -> Maybe String
reverseLookupRef p (Refs m) = case DataMap.keys $ DataMap.filter (== p) m of
    []    -> Nothing
    (k:_) -> Just k

newRef :: String -> Expr a -> Refs a
newRef key value = Refs $ DataMap.singleton key value

emptyRef :: Refs a
emptyRef = Refs DataMap.empty

union :: Refs a -> Refs a -> Refs a
union (Refs m1) (Refs m2) = Refs $ DataMap.union m1 m2 

hasRecursion :: Refs a -> Bool
hasRecursion refs = hasRec refs (DataSet.singleton "main") (lookupRef refs "main")

hasRec :: Refs a -> DataSet.Set String -> Expr a -> Bool
hasRec _ _ Empty = False
hasRec _ _ EmptySet = False
hasRec _ _ (Node _ _) = False
hasRec refs set (Or l r) = hasRec refs set l || hasRec refs set r
hasRec refs set (And l r) = hasRec refs set l || hasRec refs set r
hasRec refs set (Not p) = hasRec refs set p
hasRec refs set (Concat l r) = hasRec refs set l || (nullable refs l && hasRec refs set r)
hasRec refs set (Interleave l r) = hasRec refs set l || hasRec refs set r
hasRec _ _ (ZeroOrMore _) = False
hasRec refs set (Optional p) = hasRec refs set p
hasRec refs set (Contains p) = hasRec refs set p
hasRec refs set (Reference name) = DataSet.member name set || hasRec refs (DataSet.insert name set) (lookupRef refs name)
