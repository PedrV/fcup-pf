--31
pot2 :: Int -> Int
pot2 n = product $ take n $ repeat 2

--32
--a)
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

--b)
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

--c)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

--d)
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n-1) x

--e)
(!!!) :: [a] -> Int -> a
(!!!) xs 0 = head xs
(!!!) (x:xs) n = (!!!) xs (n-1)

--f)
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) | x == n     = True
               | otherwise  = elem' n xs


--33
concat'' :: [[a]] -> [a]
concat'' (xss) = [x | xs<-xss, x<-xs]

replicate'' :: Int -> a -> [a]
replicate'' n x = [x | y<-[1..n]]

(!!!!) :: [a] -> Int -> a
(!!!!) xs k = head [a | (a,b)<- zip xs [0..], b == k]


--34
--a)
leastSquare :: Int -> Int
leastSquare x = square 0 x

square :: Int -> Int -> Int 
square n x | n*n <= x && (n+1)*(n+1) > x    = n
           | otherwise                      = square (n+1) x

--b)
isqrt :: Int -> Int
isqrt n = head [x | x<-[0..n], x*x <= n && (x+1)*(x+1) > n ]


--35
--a)
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

--b)
rangeProduct :: Int -> Int -> Int
rangeProduct a b | a < b        = a * rangeProduct (a+1) b
                 | otherwise    = a

--c)
factorialByRange :: Int -> Int
factorialByRange x = rangeProduct 1 x


--36
mdc :: Int -> Int -> Int
mdc a b | b == 0        = a
        | otherwise     = mdc b (a `mod` b)

--37
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub [y | y<-xs, y /= x]

--38
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse k [x] = [x]
intersperse k (x:xs) = x : k : intersperse k xs

--39
maxFun :: (Integer -> Integer) -> Integer -> Integer
maxFun f 0 = f 0
maxFun f n = max (f n) (maxFun f (n-1))


--40
anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f 0 = f 0 == 0
anyZero f n | f n == 0      = True
            | otherwise     = anyZero f (n-1)

--41
maxSum :: (Integer -> Integer) -> Integer -> Integer
maxSum f 0 = f 0
maxSum f n = f n + maxSum f (n-1)  

--42
--a)
insert :: Ord a => a -> [a] -> [a]
insert z [] = [z]
insert z [x] | z < x      = [z,x]
             | otherwise  = [x,z]
insert z (x:y:xs) | z <= x           = z : x : y : xs
                  | z >= x && z <= y = x : z : y : xs
                  | otherwise        = x : insert z (y:xs)

--b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

qsort :: Ord a => [a] -> [a] 
qsort [] = []
qsort (x:xs) = qsort (left) ++ [x] ++  qsort (right)
            where left = [ y | y<-xs, y <= x]
                  right = [ y | y<-xs, y > x]

--43
--a) 
minimum' :: Ord a => [a] -> a
minimum' [] = error "minimum: No minimum in a empty list"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

--b)
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete y (x:xs) | y == x    = xs
                | otherwise = x : delete y xs

--c)
ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs = m : ssort k
        where k = delete (m) xs
              m = minimum' xs

--44
--a)
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys 
merge (x:xs) (y:ys) | x <= y        = x : merge xs (y:ys)
                    | otherwise     = y : merge (x:xs) ys

--b)
metades :: [a] -> ([a],[a])
metades [] = ([],[])
metades xs = (left,right)
            where left = take l xs
                  right = drop l xs
                  l = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort l) (msort r)
          where (l,r) = metades xs

-- Hard to arbitrary number -- 
--45
bits :: Int -> [[Bool]]
bits n = [ [i,j] | i<-[True, False], j<-[True,False]] 


-- VIP --
--46
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations l = [a : x | a<-l, x<-(permutations $ filter (\x -> x /= a) l)]

