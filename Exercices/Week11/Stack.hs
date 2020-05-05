module Stack (Stack, pop, top, push, empty, isEmpty) where

data Stack a = Stack [a] deriving Show

pop :: Stack a -> Stack a
pop (Stack (_:xs)) = Stack xs
pop (Stack []) = error "Stack.pop: Empty Stack"

top :: Stack a -> a
top (Stack (x:xs)) = x
top (Stack []) = error "Stack.top: Empty Stack"

push :: a -> Stack a -> Stack a
push x (Stack (xs)) = Stack (x:xs)

empty :: Stack a
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack xs) = length xs == 0