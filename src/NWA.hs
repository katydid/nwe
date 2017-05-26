module NWA
    ( new, exec, NWA, State(..), Symbol(..), Transition(..), bottom
    ) where

import qualified Data.Set as DataSet

newtype State = State Int
    deriving (Eq, Ord, Show)

newtype Symbol = Symbol Int
    deriving (Eq, Ord, Show)

pop :: [Symbol] -> [Symbol]
pop [s] = [s]
pop (_:ss) = ss

push :: Symbol -> [Symbol] -> [Symbol]
push s ss = (s:ss)

peek :: [Symbol] -> Symbol
peek (s:_) = s

data Transition = Internal State Char State
    | Call State Char (Symbol, State)
    | Return (Symbol, State) Char State
    deriving (Eq, Ord, Show)

data NWA = NWA State [Transition] [State] [Symbol]

bottom :: Symbol
bottom = Symbol 0

new :: State -> [Transition] -> [State] -> Either String NWA
new start trans finals = case validateTrans trans of 
    (Just err) -> Left err
    Nothing -> if startExists start trans finals
        then Right $ NWA start trans finals [(Symbol 0)]
        else Left $ "start state does not exist: " ++ show start

chars :: [Transition] -> (DataSet.Set Char, DataSet.Set Char, DataSet.Set Char)
chars [] = (DataSet.empty, DataSet.empty, DataSet.empty)
chars (t:ts) = let (is, cs, rs) = chars ts
    in case t of
    (Internal _ c _) -> (c `DataSet.insert` is, cs, rs)
    (Call _ c _) -> (is, c `DataSet.insert` cs, rs)
    (Return _ c _) -> (is, cs, c `DataSet.insert` rs)

validateTrans :: [Transition] -> Maybe String
validateTrans trans = let 
    (is, cs, rs) = chars trans
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

startExists :: State -> [Transition] -> [State] -> Bool
startExists start [] fs = start `elem` fs
startExists start ((Internal s _ _):ts) fs = s == start || startExists start ts fs
startExists start ((Call s _ _):ts) fs = s == start || startExists start ts fs
startExists start ((Return (s', s) _ _):ts) fs = (s' == bottom && s == start) || startExists start ts fs

exec :: NWA -> String -> Either String Bool
exec (NWA start trans finals stack) input = case run stack start trans input of
    (Left err) -> Left err
    (Right (_, final)) -> Right (final `elem` finals)

run :: [Symbol] -> State -> [Transition] -> String -> Either String ([Symbol], State)
run stack current _ [] = Right (stack, current)
run stack current trans (c:cs) = case step stack current trans c of
    (Left err) -> Left err
    (Right (newstack, newstate)) -> run newstack newstate trans cs

step :: [Symbol] -> State -> [Transition] -> Char -> Either String ([Symbol], State)
step stack current trans c = case get trans (peek stack) current c of
    (Left err) -> Left err
    (Right (Internal _ _ next)) -> Right (stack, next)
    (Right (Call _ _ (symbol, next))) -> Right (push symbol stack, next)
    (Right (Return _ _ next)) -> Right (pop stack, next)

get :: [Transition] -> Symbol -> State -> Char -> Either String Transition
get trans symbol current input = case filter (isTrans symbol current input) trans of
    [] -> Left $ "no transition found for (" ++ show symbol ++ "," ++ show current ++ ") " ++ show input
    [t] -> Right t
    _ -> Left $ "multiple transitions found for (" ++ show symbol ++ "," ++ show current ++ ") " ++ show input

isTrans :: Symbol -> State -> Char -> Transition -> Bool
isTrans _ current input (Internal s c _) = s == current && c == input 
isTrans _ current input (Call s c _) = s == current && c == input
isTrans symbol current input (Return (s', s) c _) = s' == symbol && s == current && c == input

