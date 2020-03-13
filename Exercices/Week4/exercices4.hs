-- 31

pot2 :: (Eq a, Num a) => a -> a
pot2 0 = 1
pot2 n = 2 * pot2(n-1)

--32 / 33
--a)
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == False     = False 
            | otherwise      = and' xs

--b)
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)  | x == True     = True
            | otherwise     = or' xs

--c)
--list comp
concat' :: [[a]] -> [a]
concat' xss = [ x | xs<-xss, x<-xs]

--recursive
--cons operator (:) searchs for a element to be the head (2:[2,3]) -> funciona
-- (:) puts 2 on the head and the rest ([2,3]) has tail
-- on the other hand (++) just puts 2 things together no tail no head
concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (xs:xss) = xs ++ concat'' xss

--d)
--recursive
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a : replicate' (n-1) a

--list comp
replicate'' :: Int -> a -> [a]
replicate'' n a = [ a | x<-[0..n]]

--e)

--list comp
(!!!) :: [a] -> Int -> [a]
(!!!) xs n = let l = length xs
                in let ys = zip xs [1..l]
                    in [ x | (x,y)<-ys, y == n]


--recursive
-- (!!!!) :: [a] -> Int -> a
(!!!!) [] n = [] 
(!!!!) (x:xs) n | n == 0    = x
                | otherwise = (!!!!) xs n


--f)

-- recursive
elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) | y == x     = True
               | otherwise  = elem' y xs

-- list comp
anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue (x:xs) | x == True  = True
               | otherwise  = anyTrue xs 

elem'' :: Eq a => a -> [a] -> Bool
elem'' y xs = anyTrue [ x==y | x<-xs]


--34

--a)
leastSqrt1 :: (Num a, Ord a) => a -> a -> a
leastSqrt1 n k  | k * k > n     = k
                | otherwise     = leastSqrt1 n (k+1)

--b)
issqrt :: (Num a, Ord a, Enum a) => a -> a
issqrt n = (!!) [x | x<-[1..n], (x*x < n && (x+1)*(x+1) > n)] 0


--35

--a)
factorial :: (Eq a, Num a) => a -> a
factorial 1 = 1
factorial n = n * factorial (n-1)

--b)
rangeProduct :: (Num a, Enum a) => a -> a -> a
rangeProduct a b = product [ x | x<-[a..b]]

--c)
facRange :: (Num a, Enum a) => a -> a
facRange n = rangeProduct 1 n

--36
mdc :: Integer -> Integer -> Integer
mdc a b | b == 0    = a
        | otherwise = mdc b (mod a b)

--37

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : (nub [y | y<-xs, y /= x])
                -- nub de apenas os elementos que sao diferentes daquele que eu ja pos no inicio (x)

--38

interspace :: a -> [a] -> [a]
interspace b [x] = [x]
-- ou -- interspace b (x:xs) = [x] ++ [b] ++ interspace b xs
interspace b (x:xs) = x : b : interspace b xs 

--39

t2 a = a+2  

fapl :: (Ord a, Num a) => (a -> b) -> a -> [b]
fapl f n | n > 0     = f n : fapl f (n-1)
         | otherwise = []

max' :: Ord a => [a] -> a
max' [x] = x
max' (x:xs) | (max' xs) > x    = max' xs
            | otherwise        = x

fmax :: (Num a, Ord a) => (a -> a) -> a -> a
fmax f n = max' (fapl f n)

--40

fapl :: (Ord a, Num a) => (a -> b) -> a -> [b]
fapl f n | n > 0     = f n : fapl f (n-1)
         | otherwise = []

