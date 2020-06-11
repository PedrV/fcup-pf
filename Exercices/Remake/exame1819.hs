-- 18/20

--1

a) 3
b) [[1,2], [3]]
c) [11, 12, 13, 14, 15]
d) 240
e) []
f) False
g) [(1,1), (4,4), (9,7), (16,10), (25,13), (36,16)]
h) [ (x,y) | x<-[1,3..], y<-[10,8..] ]
i) erro - ou se fosse (op x base) -> [5,4,3,2,1] (h seria repr do foldl)
j) (Int -> Int)
k) [(Bool -> Bool) -> [Bool] -> [Bool]]
l) data Arv a = F a | N (Arv a) (Arv a)
m) g :: (a -> Bool) -> [a] -> [a] 

--2
--a)
notaF :: [Float] -> [Float] -> Float
notaF xs ys = sum [ x*y | (x,y)<- zip xs ys ]

--b)
rfc :: (Num a, Ord a) => [[a]] -> Int
rfc xss = length [ xs | xs<-xss, any (< 8) xs ]

--3
transitiva :: [(Int,Int)] -> Bool
transitiva (g:gs) = [  (k,k1)<-[ x | (x,y)<-(g:gs), (z,z1)<-gs, x == z1 ], (k2,k3)<-(g:gs),  ] 


--4
iterate :: (a -> b) -> a -> [b]
iterate f n = n : [ x | x<-(iterate f (f n)) ]

--5
--a)
deleteNth :: Int -> [a] -> [a]
deleteNth _ [] = []
deleteNth n xs = take (n-1) xs ++ deleteNth n (drop n xs) 

--b)
deleteNth :: Int -> [a] -> [a]
deleteNth _ [] = []
deleteNth 0 xs = xs
deleteNth n xs = [ x | (x,y)<- zip xs [1..], y `mod` n /= 0]

--6
--a) 
soma :: Num a => Arv a -> a
soma Folha = 0                                              (s1)
soma (No x left right) = x + (soma left) + (soma right)     (s2)

--b)
somaArv :: Arv a -> Arv a -> Arv a
somaArv Folha t2 = t2                                                           (sA1)
somaArv t1 Folha = t1                                                           (sA2)
somaArv (No x t1 t2) (No y t3 t4) = No (x+y) (somaArv t1 t3) (somaArv t2 t4)    (sA3)


--7
--a)

Objetivo: soma (somaArv t t) = 2 * soma t

Caso Base: 
        soma (somaArv Folha Folha)      (sA1)
        soma Folha                      (s1)
        0                               (elemNeutro)
        2 * 0                           (s1)
        2 * soma Folha

Caso de Indução: 

        soma (somaArv t t) = 2 * soma t => soma (somaArv (No x t1 t2) (No x t1 t2)) = 2 * soma (No x t1 t2)

Hipotese de Indução: soma (somaArv t t) = 2 * soma t

        soma (somaArv (No x t1 t2) (No x t1 t2))                    (sA2)
        soma (No (x+x) (somaArv t1 t1) (somaArv t2 t2))             (s2)    
        (x+x) + soma (somaArv t1 t1) + soma (somaArv t2 t2)         H.I
        2 * x + 2 * soma t1 + 2 * soma t2                           (evi)
        2 * (x + soma t1 + soma t2)                                 (s2)
        2 * soma (No x t1 t2)