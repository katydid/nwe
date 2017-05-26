module NWA
    ( new, exec, NWA, State(..), Symbol(..), Transition(..)
    ) where

import qualified Data.Set as DataSet

newtype State = State Int
    deriving (Eq, Ord, Show)

data Symbol = Symbol Int
    | Bottom
    deriving (Eq, Ord, Show)

pop :: [Symbol] -> [Symbol]
pop [Bottom] = [Bottom]
pop (_:ss) = ss

push :: Symbol -> [Symbol] -> [Symbol]
push s ss = (s:ss)

peek :: [Symbol] -> Symbol
peek (s:_) = s

data Transition a = Internal State a State
    | Call State a (Symbol, State)
    | Return (Symbol, State) a State
    deriving (Eq, Ord, Show)

data NWA a = NWA State [Transition a] [State] [Symbol]

new :: (Show a, Eq a, Ord a) => State -> [Transition a] -> [State] -> Either String (NWA a)
new start trans finals = case validateTrans trans of 
    (Just err) -> Left err
    Nothing -> if startExists start trans finals
        then Right $ NWA start trans finals [Bottom]
        else Left $ "start state does not exist: " ++ show start

alphabets :: (Show a, Eq a, Ord a) => [Transition a] -> (DataSet.Set a, DataSet.Set a, DataSet.Set a)
alphabets [] = (DataSet.empty, DataSet.empty, DataSet.empty)
alphabets (t:ts) = let (is, cs, rs) = alphabets ts
    in case t of
    (Internal _ c _) -> (c `DataSet.insert` is, cs, rs)
    (Call _ c _) -> (is, c `DataSet.insert` cs, rs)
    (Return _ c _) -> (is, cs, c `DataSet.insert` rs)

validateTrans :: (Show a, Eq a, Ord a) => [Transition a] -> Maybe String
validateTrans trans = let 
    (is, cs, rs) = alphabets trans
    ics = is `DataSet.intersection` cs
    irs = is `DataSet.intersection` rs
    crs = cs `DataSet.intersection` rs
    in if (not $ DataSet.null ics)
    then Just $ "overlapping input alphabet for internals and calls: " ++ show ics
    else if (not $ DataSet.null irs)
        then Just $ "overlapping input alphabet for internals and returns: " ++ show irs
        else if (not $ DataSet.null crs)
            then Just $ "overlapping input alphabet for calls and returns: " ++ show crs
            else Nothing

startExists :: State -> [Transition a] -> [State] -> Bool
startExists start [] fs = start `elem` fs
startExists start ((Internal s _ _):ts) fs = s == start || startExists start ts fs
startExists start ((Call s _ _):ts) fs = s == start || startExists start ts fs
startExists start ((Return (s', s) _ _):ts) fs = (s' == Bottom && s == start) || startExists start ts fs

exec :: (Show a, Eq a) => (NWA a) -> [a] -> Either String Bool
exec (NWA start trans finals stack) input = case run stack start trans input of
    (Left err) -> Left err
    (Right (_, final)) -> Right (final `elem` finals)

run :: (Show a, Eq a) => [Symbol] -> State -> [Transition a] -> [a] -> Either String ([Symbol], State)
run stack current _ [] = Right (stack, current)
run stack current trans (c:cs) = case step stack current trans c of
    (Left err) -> Left err
    (Right (newstack, newstate)) -> run newstack newstate trans cs

step :: (Show a, Eq a) => [Symbol] -> State -> [Transition a] -> a -> Either String ([Symbol], State)
step stack current trans c = case get trans (peek stack) current c of
    (Left err) -> Left err
    (Right (Internal _ _ next)) -> Right (stack, next)
    (Right (Call _ _ (symbol, next))) -> Right (push symbol stack, next)
    (Right (Return _ _ next)) -> Right (pop stack, next)

get :: (Show a, Eq a) => [Transition a] -> Symbol -> State -> a -> Either String (Transition a)
get trans symbol current input = case filter (isTrans symbol current input) trans of
    [] -> Left $ "no transition found for (" ++ show symbol ++ "," ++ show current ++ ") " ++ show input
    [t] -> Right t
    _ -> Left $ "multiple transitions found for (" ++ show symbol ++ "," ++ show current ++ ") " ++ show input

isTrans :: (Eq a) => Symbol -> State -> a -> Transition a -> Bool
isTrans _ current input (Internal s c _) = s == current && c == input 
isTrans _ current input (Call s c _) = s == current && c == input
isTrans symbol current input (Return (s', s) c _) = s' == symbol && s == current && c == input

