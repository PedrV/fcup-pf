--47

listcomp :: (a -> a) -> (a -> Bool) -> [a] -> [a]
listcomp f p xs = filter p $ map f xs

--48

--a)

(++++) :: [a] -> [a] -> [a]
(++++) xs ys = (foldr (:) ys xs)

{- Extra -}
foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' f n [] = n 
foldr' f n (x:xs) = f x (foldr' f n xs)

foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' f n [] = n
foldl' f n (x:xs) = foldl' f (f n x) xs

-- quando se acaba a 1º lista, ys passa a ser a lista base que vai receber
-- os elementos de xs um de cada vez há cabeça e.g [1,2,3] +++ [4,5] = 1:2:3:[4,5]
(+++) []     ys = ys 
(+++) (x:xs) ys = x : xs +++ ys

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x] 

{- End Extra -}

--b)
concat' :: [[a]] -> [a]
concat' (xs:xss) = foldl (++) xs xss

concat'' :: [[a]] -> [a]
concat'' (xs:xss) = foldr (++) xs xss

--c)
reverse1 :: [a] -> [a]
reverse1 xs = foldr (\b c -> c ++ [b] ) [] xs  -- c vai ser []

--d)
reverse'' :: [a] -> [a]
reverse'' xs = foldl (\x y -> y:x) [] xs

--e)
elem' :: Eq a => a -> [a] -> Bool
elem' n xs = any (n==) xs 


--49

numb 0 _ = 1
numb n x = 10 * numb (n-1) 10

dec2Int :: [Int] -> Int
dec2Int [] = 0
dec2Int (x:xs) = x * (numb size 10) + dec2Int xs
                where size = length (xs)

dec2Int' :: [Int] -> Int
dec2Int' [] = 0
dec2Int' (x:xs) = foldl (\x y -> x*10 + y ) 0  (x:xs)

--50

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys


--51

-- taked from 42 a)
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n [x] | n >= x      = [x] ++ [n]     
              | n < x       = [n] ++ [x]
insert' n (x:y:xs) | n >= x && n <= y    = x : n : y : xs
                   | n <= x              = n : x : y : xs  
                   | otherwise           = x : y : insert' n xs


isort :: Ord a => [a] -> [a]
isort (x:xs) = foldr insert' [] (x:xs)


--52

{- EXTRA from week 5 -}
comb :: Eq a => [a] -> [[a]]
comb xs = [ [y,z,k] |  y<-xs,z<-xs,k<-xs, (y /= z) && (y /= k) && (k /= z)] 

--a)

-- trade first and last element positions
shift :: [a] -> [a]
shift []     = []
shift (x:xs) = y : reverse ys
              where (y:ys) = reverse (x:xs)

-- put first in the last pos
shift' :: [a] -> [a]
shift' []     = []
shift' (x:xs) = foldr (:) [x] xs

--b)

rotate :: [a] -> [[a]]
rotate xs = foldr (\x y -> y ++ [shift' (last y)]) [xs]  (tail xs)


--53

--a)
maximum' :: Ord a => [a] -> a
maximum' (x:xs) = foldr1 (\x y -> if x > y then x else y) (x:xs)

maximum'' :: Ord a => [a] -> a
maximum'' (x:xs) = foldl1 (\x y -> if x > y then x else y) (x:xs)

minimum' :: Ord a => [a] -> a
minimum' (x:xs) = foldr1 (\x y -> if x > y then y else x) (x:xs)

minimum'' :: Ord a => [a] -> a
minimum'' (x:xs) = foldl1 (\x y -> if x > y then y else x) (x:xs)

--b)

init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x : init' xs

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last xs

head' :: [a] -> a
head' [x] = x
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [x] = [x]
tail' (x:xs) = xs

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f xs = foldr' f (last' xs) (init' xs)

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f xs = foldl' f (head' xs) (tail' xs)

--54

--a)
add ::  (Enum a, Num a) => a -> a -> a
add i 0 = i
add i j = succ (add i (pred j))

mult :: (Enum a, Num a) => a -> a -> a
mult i 0 = 0
mult i j = add i (mult i (pred j))

exp' :: (Enum a, Num a) => a -> a -> a
exp' i 0 = 1
exp' i j = mult i (exp' i (pred j))

next' :: (Enum a, Num a) => a -> a -> a
next' i 0 = i
next' i j = exp' i (next' i (pred j))


--b)
foldi :: (a -> a) -> a -> Integer -> a 
foldi f q 0 = q
foldi f q i = f (foldi f q (pred i)) 

add' :: Integer -> Integer -> Integer
add' i j | j < 0 && i > 0   = foldi succ j i
         | i < 0 && j > 0   = foldi succ i j
         | otherwise        = error "invalid"

mult' :: Integer -> Integer -> Integer
mult' 0 j = 0
mult' i 0 = 0
mult' 1 j = j
mult' i 1 = i
mult' i j = (foldi succ i i) + mult' i (pred j)

--c)


--55 

mdc a b = fst $ until (\(a,b) -> b == 0) (\(a,b) -> (b, a`mod`b)) (a,b)

--56

scanl' :: (a -> a -> a) -> a -> [a] -> [a]
scanl' f n [] = []
scanl' f n (x:xs) = [(foldl f n (x:xs))] ++ (scanl' f n xs) 

scanl'' :: Num a => (a -> a -> a) -> a -> [a] -> [a]
scanl'' f n xs = reverse $ scanl' f n (reverse xs)
