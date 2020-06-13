--47
func :: (a -> b) -> (a -> Bool) -> [a] -> [b]
func f p xs =  map f (filter p xs)  

--48

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f n [] = n
foldr' f n (x:xs) = f x (foldr' f n xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f n [] = n
foldl' f n (x:xs) = foldl' f (f n x) xs

--a)
(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs 

--b)
concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss

--c)
reverse' :: [a] -> [a]
reverse' xs = foldr (\a b -> b ++ [a]) [] xs

--d)
reverse'' :: [a] -> [a]
reverse'' xs = foldl (\b a -> [a] ++ b) [] xs

--e)
elem' :: Eq a => a -> [a] -> Bool
elem' x xs = any (\a -> a == x) xs 

--49
dec2int :: [Int] -> Int
dec2int (xs) = foldl (\b a -> b*10 + a) 0 xs  

--50
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs [] = []
zipWith' f [] ys = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

--51

insert :: Ord a => a -> [a] -> [a]
insert z [] = [z]
insert z [x] | z < x      = [z,x]
             | otherwise  = [x,z]
insert z (x:y:xs) | z <= x           = z : x : y : xs
                  | z >= x && z <= y = x : z : y : xs
                  | otherwise        = x : insert z (y:xs)   

isort :: Ord a => [a] -> [a]
isort xs = foldr (\a b -> insert a b) [] xs

--52

--a)
shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

--b)
rotate' :: [a] -> [[a]]
rotate' (x:xs) = foldr (\a b -> shift (head b) : b ) [x:xs] (xs)

rotate :: [a] -> [[a]]
rotate xs = rotateWorker (length xs) xs

rotateWorker :: Int -> [a] -> [[a]]
rotateWorker 0 _ = [[]]
rotateWorker 1 xs = [xs]
rotateWorker n (x:xs) = (x:xs) : rotateWorker (n-1) l
                        where l = xs ++ [x]


--53
--a)
minimum' :: Ord a => [a] -> a
minimum' xs = foldr1 (\a1 a2 -> if a1 < a2 then a1 else a2) xs

minimum'' :: Ord a => [a] -> a
minimum'' xs = foldl1 (\a1 a2 -> if a1 < a2 then a1 else a2) xs

maximum' :: Ord a => [a] -> a
maximum' xs = foldr1 (\a1 a2 -> if a1 > a2 then a1 else a2) xs

maximum'' :: Ord a => [a] -> a
maximum'' xs = foldl1 (\a1 a2 -> if a1 > a2 then a1 else a2) xs


--b)
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "fold1: fold1 is not defined for empty list"
foldl1' f [x] = x
foldl1' f (x:xs) = foldl1' f ((f x (head xs)):(tail xs))

foldl1'' :: (a -> a -> a) -> [a] -> a
foldl1'' f xs = foldl f (head xs) (tail xs)


foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = error "folr1: folr1 is not defined for empty list"
foldr1' f [x] = x
foldr1' f (xs) = f (head xs) (foldr1' f (tail xs))

foldr1'' :: (a -> a -> a) -> [a] -> a
foldr1'' f xs = foldr f (last xs) (init xs)


--54
--a)
succ' i = i + 1
pred' i = i - 1

add i 0 = i
add i j = succ' (add i (pred' j))

mult i 0 = 0
mult i 1 = i
mult i j = add i (mult i (pred' j))

exp' i 0 = 0
exp' i 1 = i
exp' i j = mult i (exp' i (pred' j))

--b)
foldi :: (a -> a) -> a -> Integer -> a
foldi f q 0 = q
foldi f q i = f (foldi f q (pred i))

add' i j = foldi succ' i j 
mult' i j = foldi (\x -> add' x i) i (pred j) 
exp'' i j = foldi (\x -> mult' x i) i (pred j)

--c)


--55
mdc a b = fst $ until (\(a,b) -> b == 0) (\(a,b) -> (b, a `mod` b) ) (a,b)

--56
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f n [] = [n]
scanl' f n (x:xs) = [n] ++ scanl' f (f n x) xs

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ x [] = [x]
scanr' f x (y:ys) = (f y (head partialResult)) : partialResult
    where partialResult = scanr' f x ys