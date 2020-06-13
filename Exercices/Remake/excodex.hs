forte :: String -> Bool
forte ss = ((length ss) >= 6) && alg && mais && mins
         where alg = or [ s >= '0' && s <= '9' | s<-ss ]
               mais = or [ s >= 'A' && s <= 'Z' | s<-ss ]
               mins = or [ s >= 'a' && s <= 'z' | s<-ss ]

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) | x == False   = False
             | otherwise    = myand xs

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) | x == True     = True
            | otherwise     = myor xs

interspace :: a -> [a] -> [a]
interspace _ [] = []
interspace n [x] = [x]
interspace n (x:xs) = x : n : interspace n xs

anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f 0 = (f 0) == 0
anyZero f n | (f n) == 0        = True
            | otherwise         = anyZero f (n-1)

anyZero' :: (Integer -> Integer) -> Integer -> Bool
anyZero' f n = any (== 0) [ (f x) | x<-[n,n-1..0] ]

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n <= x    = n : x : xs
                | n >  x    = x : insert n xs

isort :: Ord a => [a] -> [a]
isort [] = [] 
isort (x:xs) = insert x (isort xs)

minimum' :: Ord a => [a] -> a
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

minimum'' :: Ord a => [a] -> a
minimum'' xs = foldr (\a b -> if a <= b then a else b ) (last xs) xs

-----------
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)
-----------

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete n (x:xs) | x == n        = xs
                | otherwise     = x : delete n xs


isFirst :: Eq a => a -> [a] -> [Bool]
isFirst _ [] = []
isFirst n (x:xs) | x == n       = True : [ False | x<-[1..(length xs)] ]
                 | otherwise    = False : isFirst n xs

delete' :: Eq a => a -> [a] -> [a]
delete' n xs =  [ x | (x,y)<- zip xs (isFirst n xs), x /= n || y == False ]

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y        = x : merge xs (y:ys)
                    | y < x        = y : merge (x:xs) ys 
                    | otherwise    = x : y : merge xs ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right) 
         where left = take m xs
               right =  drop m xs
               m = (length xs) `div` 2 


permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations l = [ a:x | a<-l, x<-(permutations $ filter (\x -> x /= a) l)] 

-- Versao mais op que suporta repetidos --
permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute xs = concatMap (\x -> map (x:) $ permute $ delete x xs) xs


(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (\a b -> a : b) ys xs

reverse' :: [a] -> [a]
reverse' xs = foldr (\a b -> b ++ [a] ) [] xs

dec2Int :: [Int] -> Int
dec2Int xs = foldl (\b a -> b*10 + a) 0 xs 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] ys = []
zipWith' f xs [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys


shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

rotate :: [a] -> [[a]]
rotate xs = foldr (\a b -> shift (head b) : b) [xs] (tail xs)

--ou 

rotate' :: [a] -> [[a]]
rotate' xs = foldr (\a b -> b ++ [shift (last b)]) [xs] (tail xs)


maximum' :: Ord a => [a] -> a 
maximum' xs = foldr1 (\a b -> if a > b then a else b) xs


until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x | p x       = f x
             | otherwise = until' p f (f x)

mdc :: Integer -> Integer -> Integer 
mdc a b = fst $ until' (\(a,b) -> b == 0) (\(a,b) -> (b,a `mod` b)) (a,b)

fact = 1 : zipWith (*) fact [1..] 
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


mergeUniq :: Ord a => [a] -> [a] -> [a] 
mergeUniq xs [] = xs 
mergeUniq [] ys = ys
mergeUniq (x:xs) (y:ys) | x < y     = x : mergeUniq xs (y:ys)
                        | x > y     = y : mergeUniq (x:xs) ys
                        | otherwise = x : mergeUniq xs ys 

hamming = 1 : mergeUniq l5 (mergeUniq l2 l3)
        where l5 = map (*5) hamming
              l3 = map (*3) hamming
              l2 = map (*2) hamming

nextpascal :: [Int] -> [Int]
nextpascal xs = [1] ++ zipWith (+) xs (tail xs) ++ [1]

pascal = [[1]] ++ map nextpascal pascal

strings = [""] ++ [ y:x | x<-strings, y<-['a'..'z'] ] 


data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Eq,Show,Ord)

sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No x esq dir) = x + (sumArv esq) + (sumArv dir)

listadecr :: Arv a -> [a]
listadecr Vazia = []
listadecr (No x esq dir) = (listadecr dir) ++ [x] ++ (listadecr esq)

nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No x l r) = [x]
nivel n (No x l r) = (nivel (n-1) l) ++ (nivel (n-1) r)

mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ left _) =  mais_esq left

mais_dir :: Arv a -> a
mais_dir (No x _ Vazia) = x
mais_dir (No _ _ right) = mais_dir right  

remover :: Ord a => a -> Arv a -> Arv a
remover _ Vazia = Vazia
remover n (No x Vazia right) = if x == n then right else No x Vazia (remover n right)
remover n (No x left Vazia) = if x == n then left else No x (remover n left) Vazia
remover n (No x left right) |  n < x        = No x (remover n left) right
                            |  n > x        = No x left (remover n right)
                            | otherwise     = No y (remover y left) right
                            where y = mais_dir left
{- 
[] : [] = [[]]
[] ++ [] = []
[[]] ++ [[]] = [[],[]]
[] : [[]] = [[],[]] 
-}

rot13 :: IO ()
rot13 = do
    content <- getContents
    let line = lines (content)
    if (null line) then
        return ()
        else do
            exe line
            return()

exe :: [String] -> IO ()
exe (x:xs) = do
    if (null xs) then do 
        putStrLn (rot13String x)
        return ()
        else do
            putStrLn (rot13String x)
            exe xs
            return ()

rot13String :: String -> String
rot13String [] = []
rot13String (s:ss) = if all (== False) [(elem s ['a'..'m']), (elem s ['A'..'M'])] &&  all (== False) [(elem s ['m'..'z']), (elem s ['M'..'Z'])] then s : rot13String ss 
                     else if ( or [(elem s ['a'..'m']), (elem s ['A'..'M'])] ) then
                          foldl (\a _ -> succ a) s [1..13] : rot13String ss
                          else foldl (\a _ -> pred a) s [1..13] : rot13String ss
