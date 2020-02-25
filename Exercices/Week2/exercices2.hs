{- Haskell Basics -}

--14
--a) [Char] -- List of chars
--b) (Char, Char, Char) -- Tuple of 3 chars
--c) [(True,Char)]  -- List of tuples, first bool, second char
--d) ([Bool], [Char]) -- Tuple of lists of bools and lists of chars
--e) [([a] -> [a])]  -- List of functions that takes lists and returns lists
--f) [(a -> a)]  -- List of functions that receive an argument and return an argument of the same type





{- Extra -}

map' :: (a -> a) -> [a] -> [a]
map' f [] = []
map' f (x:xs) = f x : map' f xs

mult2 :: Num a => [a] -> [a]
mult2 xs = map' (\x -> 2*x) xs 

mult2' :: Num a => [a] -> [a]
mult2' [] = []
mult2' (x:xs) = 2*x : mult2' xs

times3 :: Num a => ([a] -> [a]) -> [a] -> [a]
times3 f x = f (f (f x))


concat :: [[a]] -> [a]
concat xss = [x | xs<-xss, x<-xs]