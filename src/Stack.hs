module Stack where

data Symbol a = Symbol a
    | Bottom
    deriving (Eq, Ord, Show)

type Stack a = [Symbol a]

pop :: Stack a -> Stack a
pop [Bottom] = [Bottom]
pop (_:ss) = ss

push :: Symbol a -> Stack a -> Stack a
push s ss = (s:ss)

peek :: Stack a -> Symbol a
peek = head