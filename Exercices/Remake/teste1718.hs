-- 18/20 

--1
a) [[1,2,3],[4],[5]]
b) 5
c) [8,6,4,2,0]
d) 9
e) [(1,1),(2,1),(3,1),(4,1),(2,2),(3,2),(4,2)]
f) [2,4,8,16,32]
g) serie = 1 : [ 2*x+1 | x<-serie ]
h) 15
i) ([Bool],[Char])
j) p :: a -> b -> (a,b)
k) h :: Eq a => [a] -> [a] -> [a] 
l) feql :: Eq a => [a] -> Bool

--2
--a)
distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (x1,y1) (x2,y2) =  sqrt ((x2-x1)^2 + (y2-y1)^2)

--b)
colineares :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
colineares (x1,y1) (x2,y2) (x3,y3) = d1 == d2
                                   where d1 = (y2-y1) / (x2-x1)
                                         d2 = (y3-y2) / (x3-x2)

--3
--a)
niguais :: Int -> a -> [a]
niguais 0 _ = []
niguais n x = x : niguais (n-1) x

--b)
niguais :: Int -> a -> [a]
niguais n x = [ x | y<-[1..n] ]


--4
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y         = x : merge xs (y:xs)
                    | x > y         = y : merge (x:xs) ys
                    | otherwise     = x : y merge xs ys

--5
length_zip :: [a] -> [(Int,a)]
length_zip xs = [ (x,y) | (x,y)<- zip [l,l-1..1] xs ]
              where l = length xs

--6

resto :: Int -> Int -> Int
resto n x = n - x

decompor :: Int -> [Int] -> [([Int],Int)]
decompor _ [] = []
decompor n (x:xs) | r < 0         = decompor n xs
                  | r > 0         = (x,r) : decompor n (x:xs)
                  | otherwise     = [(x,r)]
                  where r = resto n x  


test xs = if snd $ last xs == 0 then [ x | (x,y)<-xs ] else test $ decompor 
2,23 : 2,21 : 2,19 : 2,17 : 2,15 : 2,13 : 2,11 : 2,9 : 2,7 : 2,5 : 2,3 : 2,1


