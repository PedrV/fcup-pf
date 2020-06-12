-- 18/20

--1
a) [[1,2],[],[3,4],[5]]
b) [5]
c) 2
d) [16,20,24,28,32]
e) [(3,2),(4,3),(5,4),(5,6),(6,8),(7,12)]
f) [[2,8],[4,6],[]]
g) [ (x,y) | (x,y)<- zip [0,1..6] [6,5..0] ]
h) 15
i) [(Char, String)]
j) (Num a, Ord a, Num b, Ord b) => b -> [a] -> Bool
k) ig :: [Int] -> Bool 
l) fix :: Eq a => ( a -> a ) -> a -> Bool 

--2
--a)
pitagoricos :: Int -> Int -> Int -> Bool
pitagoricos a b c = (a^2 + b^2 == c^2) || (b^2 + c^2 == a^2) || (a^2 + c^2 == b^2)

--b)
hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt (a^2 + b^2)

--3
--a)
diferentes :: Eq a => [a] -> [a] 
diferentes [] = []  
diferentes [x] = []
diferentes (x:y:xs) | x == y        = diferentes (y:xs)
                    | otherwise     = x : diferentes (y:xs)


--b)
diferentes :: Eq a => [a] -> [a]
diferentes xs = [ a | (a,b)<- zip xs (tail xs) , a /= b ]

--3
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs = [ (x,y,z) | ((x,y),z)<-zip (zip xs ys) zs ]

--4
partir :: Eq a => a -> [a] -> ([a],[a])
partir _ [] = ([],[])
partir n (x:xs) | n == x    = ([], x:xs)
                | otherwise = (x : (fst pair), snd pair)
                where pair = partir n xs

--5
parts:: [a] -> [[[a]]]
parts [] = [[]]
parts (x:xs) = [ [x]:ps | ps <- pss] ++ [ (x:p):ps| (p:ps) <- pss]
   where pss = parts xs
   
ou

parts:: [a] -> [[[a]]]
parts xs = [ (take n xs):ps | n <- [1..length xs], ps <- parts (drop n xs)]