--57

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = [] 
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

factorial = 1 : zipWith' (*) factorial [1..]

fibs = 0 : 1 : zipWith' (+) fibs (tail fibs)


--58

{- -!EXTRA!-
--get factor primes of all numbers of a list in a list of lists
facPrimesList [] = []
facPrimesList (n:ns) = [(facPrimes n)] ++ facPrimesList ns 
   -!EXTRA!- -}

merge :: Ord a => [a] -> [a] -> [a]
merge [] _ = []
merge _ [] = []
merge (x:xs) (y:ys) | x <  y     = x : y : merge xs ys
                    | x > y      = y : x : merge xs ys 


primes = filterPrimes [2..]
        where filterPrimes (p:xs) = p : filterPrimes [ x | x<-xs, x `mod` p /= 0]

--get factor primes of number
facPrimes n | n > 1     = x : facPrimes (n `div` x)
            | n <= 1    = [1]   
            where x = head [ x | x<-primes, n `mod` x == 0 ]

                    
isHam n = all (<= 5) $ facPrimes n

--59

soma :: [Int] -> [Int]
soma [x,y] = [x+y]
soma (x:y:xs) = x : soma ((x+y):xs)


--60
--a)

-- binom imported from 6 a)
binom :: (Fractional a, Enum a) => a -> a -> a
binom a b = product [1..a] / (product [1..b] * product [1..(a-b)])

linha :: (Eq a, Fractional a, Enum a) => a -> [a]
linha n = y
        where y = [ binom n x | x<-[0..n] ]  

pascal = pascal [0..]
        where pascal (x:xs) = [linha x] ++ pascal xs

--b) era só mudar a forma de calcular binom


--61

-- put first in the last pos
shift :: [a] -> [a]
shift []     = []
shift (x:xs) = foldr (:) [x] xs

rotate xs =  xs : rotate (shift xs)
 


 --62 

 --a)
strings = [x : y | y <-"" : strings, x <- ['a'..'z']]

--b
stringsN n = string strings
            where string (x:xs) | (length x) == n      = x : string xs    
                                | (length x) < n      = string xs
                                | (length x) > n      = []  


--63

scanll :: (a -> b -> a) -> a -> [b] -> [a]
scanll f n (x:xs) = n : scanll f (f n x) xs


{- Extra -}

repeat' :: a -> Int -> [a]
repeat' x 0 = []
repeat' x n = x : repeat' x (n-1)

repeatpre :: a -> [a]
repeatpre x  = x : repeatpre x


cycle' :: [a] -> Int -> [a]
cycle' xs 0 = []
cycle' xs n = xs ++ cycle' xs (n-1) 

cyclepred :: [a] -> [a]
cyclepred xs = xs ++ cyclepred xs



