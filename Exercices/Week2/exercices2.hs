{- Haskell Types -}

--14
--a) [Char] -- List of chars
--b) (Char, Char, Char) -- Tuple of 3 chars
--c) [(True,Char)]  -- List of tuples, first bool, second char
--d) ([Bool], [Char]) -- Tuple of lists of bools and lists of chars
--e) [([a] -> [a])]  -- List of functions that takes lists and returns lists
--f) [(a -> a)]  -- List of functions that receive an argument and return an argument of the same type

--15
--16

--17
--a)
segundo :: [a] -> a
segundo xs = head (tail xs)
--b)
trocar :: (a,b) -> (b,a)
trocar (x, y) = (y, x)
--c)
par :: a -> b -> (a,b)
par x y = (x, y)
--d)
dobro :: Num a => a -> a
dobro x = 2 * x
--e)
metade :: Fractional a => a -> a
metade x = x/2
--f)
minuscula :: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'
--g) -- | originalmente: (Num a, Ord a) => a -> a -> a -> Bool ,
     -- | mas nao tem de ser numerico, nao estava errado mas nao era o mais amplo
intervalo :: Ord a => a -> a -> a -> Bool
intervalo x a b = x >= a && x <= b
--h)
palindromo :: Eq a => [a] -> Bool
palindromo xs = reverse xs == xs
--i)
twice :: (a -> a) -> a -> a
twice f x = f (f x)

--18
incr :: Num a => a -> a
incr x = x+1

quadrado :: Num a => a -> a
quadrado x = x*x

dobro' :: Num a => a -> a
dobro' x=x+x

media :: Fractional a => a -> a -> a
media x y = (x+y)/2

triangulo :: (Ord a, Num a) => a -> a -> a -> Bool
triangulo x y z 
            | a && b && c     = True
            | otherwise     = False      
            where a = if x + y >= z then True else False
                  b = if x + z >= y then True else False
                  c = if y + z >= x then True else False

--19
--a)
functionA :: Int -> (Int -> Int) -> Int
functionA a f = f a
--b)
functionB :: Char -> Bool -> Bool
functionB a b
            | a == 'a' && b     = True
            | otherwise         = False
--c)
functionC :: (Char -> Char -> Int) -> Char -> Int
functionC f a = if f a a == 1 then 1 else 0 

testefunctionC :: Char -> Char -> Int
testefunctionC a b = if a == b then 1 else 0

--d)
functionD :: Eq a => a -> [a] -> Bool
functionD _ [] = False
functionD y (x:xs) = if y==x then True else functionD y xs

--e)
functionE :: Eq a => [a] -> a -> [a]
functionE xs y = [x | x<-xs, y==x]

--f)
functionF :: Ord a => a -> a -> a
functionF x y
            | x > y     = x
            | x < y     = y
            | otherwise = x

--20
-- (2,[3]) pode ser passado há funcao resultado depende da funcao
-- (2,[]) pode ser passado há funcao resultado depende da funcao,
-- | (tem ou nao como lidar com empty list) - neste caso sim
-- (2,[False]) nao pode ser passado, erro de tipos

f :: Eq a => (a,[a]) -> Bool
f (_,[]) = False
f (y,xs) = if y == head xs then True else False

--21
-- (2,[3]) pode ser passado há funcao resultado depende da funcao
-- (2,[]) pode ser passado há funcao resultado depende da funcao
-- | (tem ou nao como lidar com empty list) - neste caso nao
-- (2,[False]) nao pode ser passado, erro de tipos

f1 :: Eq a => (a,[a]) -> a
f1 (y,xs) = if y == head xs then y else y

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
times3 f x = f (f x)

inList :: Eq a => [a] -> a -> [a]
inList xs y = [x | x<-xs, y==x]

concat :: [[a]] -> [a]
concat xss = [x | xs<-xss, x<-xs]