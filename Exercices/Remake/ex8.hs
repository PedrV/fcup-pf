--57
fact = 1 : zipWith (*) fact [1..]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--58
pot n = [n*x | x<-[1..]]

mergeUniq :: Ord a => [a] -> [a] -> [a]
mergeUniq xs [] = xs
mergeUniq [] ys = ys
mergeUniq (x:xs) (y:ys) | x < y     = x : mergeUniq xs (y:ys)
                        | x == y    = x : mergeUniq xs ys
                        | otherwise = y : mergeUniq (x:xs) ys 

hamming = 1 : mergeUniq l5 (mergeUniq l2 l3) 
        where l5 = map (*5) hamming
              l2 = map (*2) hamming
              l3 = map (*3) hamming

--59
soma :: [Int] -> [Int]
soma xs = 0 : zipWith (+) (soma xs) xs 

soma' :: [Int] -> [Int]
soma' (x:xs) = 0 : map (x+) (soma' xs)

--60

nextPascal :: [Int] -> [Int]
nextPascal xs = [1] ++  zipWith (+) xs (tail xs) ++ [1]

pascal = [[1]] ++ map nextPascal pascal

--61
shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

rotate :: [Int] -> [[Int]]
rotate xs = xs : rotate (shift xs) 


--62

--a)
strings = [""] ++ [ x:y | y<-strings, x<-['a'..'z']]
--strings' = [x : y | y <-"" : strings', x <- ['a'..'z']]

--b)
stringN :: Int -> [String]
stringN n = filter (\s -> length s == n) (takeWhile (\s -> (length s) <= n) strings)


--63

scanll :: (b -> a -> b) -> b -> [a] -> [b]
scanll f n (x:xs) = [n] ++ scanll f (f n x) xs

scanrr :: (a -> b -> b) -> b -> [a] -> [b]
scanrr f n [] = [n]
scanrr f n (x:xs) = (f x (head app)) : app
                    where app = scanrr f n xs




{- fact = 1 : zipWith (*) fact [1..]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = [] 
merge xs [] = xs 
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | x > y     = y : merge (x:xs) ys
                    | otherwise = x : merge xs ys

hamming = 1 : merge l5 (merge l2 l3)
        where l5 = map (*5) hamming 
              l3 = map (*3) hamming
              l2 = map (*2) hamming
              

somas :: [Int] -> [Int]
somas xs = 0 : zipWith (+) xs (somas xs)


nextPascal :: [Int] -> [Int]
nextPascal [] = []
nextPascal xs = [1] ++ zipWith (+) xs (tail xs) ++ [1]

pascal = [[1]] ++ map nextPascal pascal

shift :: [a] -> [a] 
shift (x:xs) = xs ++ [x]

rotate :: [a] -> [a] 
rotate xs = xs ++ rotate (shift xs)

strings = [""] ++ [ x:y | y<-strings, x<-['a'..'z'] ]

stringN :: Int -> [String]
stringN n = filter (\s -> length s == n) (takeWhile (\s -> length s <= n) strings)


scanll :: (b -> a -> b) -> b -> [a] -> [b]
scanll f n [] = [n]
scanll f n (x:xs) = [n] ++ scanll f (f n x) xs  -}