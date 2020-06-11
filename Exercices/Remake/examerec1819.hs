-- 17,5/20

--1
a) [[],[],[],[]]
b) 4
c) [0,0,0,1,1,1,1,2,2,2]
d) [1,3,9,27,81]
e) [4,8]
f) [2,4..20]
g) [ x | ] -- Errado [if even x then 2*x else 2ˆx-1 | x <-[0..]]
h) False
i) [Int -> Bool]
j) [Char] -> Int
k) data Arv = Vazia | No (Arv) Int (Arv)
l) flip :: (a -> b -> c) -> b -> a -> c

--2
--a)
imparDiv3 :: Num a => [a] -> Bool
imparDiv3 xs = and [ odd x | x<-xs, x `mod` 3 == 0]

--b)
imparDiv3 :: Num a => [a] -> Bool
imparDiv3 xs = length (filter (even) (filter (\x -> x `mod` 3 == 0) xs)) == 0


--3


--4
serie = 1 : [ 2*x+y+1 | (x,y)<- zip serie [2..] ] 


--5
--a)
duplicada :: Eq a => [a] -> Bool
duplicada [] = True
duplicada [x] = False
duplicada (x:y:xs) | x == y     = duplicada xs  
                   | otherwise  = False

--b)
duplica :: [a] -> [a]
duplica xs = foldr (\a b -> a : a : b) [] xs


--6
data Arv a = Folha a | No (Arv a) (Arv a)

--a)
emOrdem :: Arv a -> [a]
emOrdem (Folha a) = [a]                                     (e1)
emOrdem (No esq dir) = emOrdem esq ++ emOrdem dir           (e2)

--c)
anyArv :: (a -> Bool) -> Arv a -> Bool
anyArv p (Folha a) = p a                                    (a1)
anyArv p (No esq dir) = anyArv p esq || anyArv p dir        (a2)

--7

any (xs++ys) = any xs || any ys     (any1)

Objetivo: anyArv p t = any p (emOrdem t)

Caso Base: 
        anyArv p (Folha a)                  (a1)
        p a                                 (any def)    
        any p [a]                           (e1)                                            
        any p (emOrdem (Folha a))

Caso de Indução: anyArv p t = any p (emOrdem t) => anyArv p (No t1 t2) = any p (emOrdem (No t1 t2))

Hipotese de Indução: anyArv p t = any p (emOrdem t)

        anyArv p (No t1 t2)                         (a2)
        anyArv p t1 || anyArv p t2                  H.I  
        any p (emOrdem t1) || any p (emOrdem t2)    (any1)
        any p ((emOrdem t1) ++ (emOrdem t2))        (e2)
        any p (emOrdem (No t1 t2))