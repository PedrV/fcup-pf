{- 14 
a) [Char] 
b) (Char,Char,Char) 
c) [(Bool,Char)]
d) ([Bool], [Char])
e) [[a] -> [a]]
f) [Bool -> Bool]
 -}

{- 15 
1) f :: (Int,Int) -> Int
2) f :: Int -> Int, g :: Int -> Int
3) f :: (Int -> Int) -> Int -> Int, g :: Int -> Int
4) f :: Int -> [Int], g :: [Int] -> Int
5) f :: (Int,Int) -> [Int -> Int]
 -}

 --16
 -- f :: (a -> a) -> Int -> [[Int]]
 -- g :: a -> a

{- 17 
a) segundo :: [a] -> a
b) trocar :: (a,b) -> (b,a)
par :: a -> b -> (a,b)
dobro :: Num a => a -> a
metade :: Fractional a => a -> a
minuscula :: Char -> Bool
intervalo :: Ord a => a -> a -> a -> Bool
paliandro :: [a] -> Bool
twice :: (a -> a) -> a -> a 
 -}

--19
--1 
f :: Int -> (Int -> Int) -> Int
f x g = g x

--2
f1 :: Char -> Bool -> Bool
f1 a p = a == 'p' && p

--3
f2 :: (Char -> Char -> Int) -> Char -> Int
f2 g1 a = g1 a a

--4
f3 :: Eq a => a -> [a] -> Bool
f3 y xs = y == (head xs)

--5
f4 :: Eq a => [a] -> [a] -> Bool
f4 xs ys = xs == ys

--6
f5 :: Ord a => a -> a -> a 
f5 x y | x > y     = y
       | otherwise = x

--20
-- f :: (a,[a]) -> Bool
-- (2,[3]) Sim, Bool 
-- (2,[]) Sim, Bool
-- (2,[True]) Não, erro de tipos entre int e bool

--21
-- f:: (a,[a]) -> a
-- (2,[3]) Sim, Int 
-- (2,[]) Sim, Int
-- (2,[True]) Não, erro de tipos entre int e bool

