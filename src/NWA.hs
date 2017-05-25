module NWA
    ( new, exec, NWA, State(..), Symbol(..), Transition(..)
    ) where

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

new :: State -> [Transition] -> [State] -> NWA
new start trans finals = NWA start trans finals [(Symbol 0)]

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
get trans symbol current input = case filter (is symbol current input) trans of
    [] -> Left $ "no transition found for (" ++ show symbol ++ "," ++ show current ++ ") " ++ show input
    [t] -> Right t
    _ -> Left $ "multiple transitions found for (" ++ show symbol ++ "," ++ show current ++ ") " ++ show input

is :: Symbol -> State -> Char -> Transition -> Bool
is _ current input (Internal s c _) = s == current && c == input 
is _ current input (Call s c _) = s == current && c == input
is symbol current input (Return (s', s) c _) = s' == symbol && s == current && c == input

