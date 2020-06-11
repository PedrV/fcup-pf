-- 18.2/20

--1
a) ["abc", "", "dce"]
b) [[2],[],[3],[4]]
c) 10
d) [2,4,6,8]
e) [1,0,-1,-2,-3]
f) [6,7,8,9,10]
g) [4,5,6]
h) [ if y `mod` 2 == 0 then -1*(2^x) else 2^x | (x,y)<- zip [1..] [0..] ] --metade
i) 24
j) f :: [a] -> a
k) ([Bool], [Char])
l) aval :: E a -> (a -> Boll) -> (Bool -> Boll -> Bool) -> Bool
m) [a] -> [a]


--2

--a)
aprov :: [Int] -> [Char]
aprov xs = [ if x >= 15 then 'A' else 'R' | x<-xs ]

--b)
injust :: [Int] -> Int
injust xs = sum [ 1 | x<-xs, x < 15 && x >= 10]


--3
repete :: [a] -> [[a]]
repete a = [] : [ a++y | y<-(repete a) ] -- metade

--4
maximo' :: [Int] -> IO ()   -- metade
maximo' xs = do
    x<-getLine
    if x == "0" then do
        putStrLn (show $ maximum xs)
        return ()
        else do
            maximo' ((read x):xs)
            return ()

maximo :: IO ()
maximo = maximo' []

--5

--a)
compL :: [(a -> a)] -> a -> a
compL [] v = v
compL xs = compL (init xs) ((last xs) v) 

--b)
compL :: [(a -> a)] -> a -> a
compL xs v = foldr (\a b -> a b) v xs

--6
--a)
soma :: Num a => Arv a -> a
soma Vazia = 0                                          (s1)
soma (No x esq dir) = x + (soma esq) + (soma dir)       (s2)
            
--b)
foldtree :: (a -> b -> b -> b) -> b -> Arv a -> b
foldtree f v Vazia = v                                                    (f1)
foldtree f v (No x esq dir) = f x (foldtree f v esq) (foldtree f v dir)   (f2)  

--7

soma3 x y z = x + y + z    (s3)

Objetivo: soma t = foldtree soma3 0 t

Caso Base:
    soma Vazia              (s1)
    0                       (f1) 
    foldtree soma3 0 Vazia

Caso de Indução: 

        soma t = foldtree soma3 0 t => soma (No x t1 t2) = foldtree soma3 0 (No x t1 t2)

Hipotese de Indução: soma t = foldtree soma3 0 t

        
        soma (No x t1 t2)                                       (s2) 
        x + (soma t1) + (soma t2)                               H.I
        x + foldtree soma3 0 t1 + foldtree soma3 0 t2           (s3) 
        soma3 0 (foldtree f v esq) (foldtree f v dir)           (f2)
        foldtree soma3 0 (No x t1 t2)

