-- 31

pot2 :: Num a => a -> a
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


-- use a suplement fucntion maybe lambda
leastSqrt :: (Num a, Ord a) => a -> a
leastSqrt n | k * k > n     = k
            | otherwise     = (+) k 1 leastSqrt n
            where k = 0