-- Exame Ano Letivo 18/19 Epoca Normal --
-- https://bit.ly/2ZGojcC

data Arv a = No a (Arv a) (Arv a) | Folha

--1
--a) 3
--b) [[1,2],[3]]
--c) [11,12,13,14,15]
--d) 240
--e) []
--f) False
--g) [(1,1), (4,4), (9,7), (16,10), (25,13), (36,16)]
--h) [(x,y) | (x,y)<- zip [1,3..] [10,8..]]
--i) erro por [] há cabeça de 1
--j) Int -> Int
--k) [(Bool -> Bool) -> [Bool] -> [Bool]]
--l) N (Arv a) (Arv a) | F a
--m) g :: (a -> Bool) -> [a] -> [a]

--2
--a) 

notaF :: Num a => [a] -> [a] -> a
notaF n p = sum [x*y | (x,y) <- zip p n]

--b)

rfc :: (Ord a, Num a) => [[a]] -> Int
rfc xs = foldr (\x y -> if any (<=8) x then y+1 else y) 0 xs


--3
type Vert = Int
type Graph = [(Vert,Vert)]

transitiva :: Graph -> Bool
transitiva [] = True
transitiva gs = and [ elem (x,w) gs | (x,y) <- gs, (z,w) <- gs, y == z]


--4

iterate' :: (Enum a, Num a) => (a -> a) -> a -> [a]
iterate' f x = x : [ f y | y <- iterate' f (x)]

--5
--a)

deleteNth :: [a] -> Int -> [a]
deleteNth xs 0 = xs
deleteNth [] n = []
deleteNth xs n = take (n-1) xs ++ deleteNth (drop n xs) n 

--b)

deleteNth' :: [a] -> Int -> [a]
deleteNth' xs 0 = xs
deleteNth' xs n = [ fst x | x<- zip xs [1..], (snd x) `mod` n /= 0 ]


--6
--a)

soma :: Num a => Arv a -> a
soma Folha = 0                                          -- (s1)
soma (No x esq dir) = x + soma esq + soma dir           -- (s2)

--b)

somaArv :: Num a => Arv a -> Arv a -> Arv a
somaArv Folha Folha = Folha                                                     -- (sArv.1)
somaArv t1 Folha = t1                                                           -- (sArv.2)
somaArv Folha t2 = t2                                                           -- (sArv.3)    
somaArv (No x t1 t2) (No y t3 t4) = No (x+y) (somaArv t1 t3) (somaArv t2 t4)    -- (sArv.4)


--7

{- Objetivo: soma (somaArv t t) = 2 * soma t

Caso Base:  soma (somaArv Folha Folha) (sArv.1)
        <=> soma (Folha)                (s1)
        <=> 0
        <=> 2 * 0                       (s1)
        <=> 2 * soma Folla
        <=> 2 * soma Folha

        [!] Caso provado

Caso de Indução: -- Supondo que isto se mantem entre 0 e n --

        soma (somaArv t t) = 2 * soma t  =>  soma (somaArv (No x t1 t2) (No x t1 t2)) = 2 * soma (No x t1 t2)

-> Hipotese de Indução: soma (somaArv t t) = 2 * soma t

        soma (somaArv (No x t1 t2) (No x t1 t2))            (sArv.4)
    <=> soma (No x+x (somaArv t1 t1) (somaArv t2 t2))       (s2)
    <=> (x+x) + soma (somaArv t1 t1) + soma (somaArv t2 t2)  H.I
    <=> (x+x) + 2 * soma t1 + 2 * soma t2                   (distro mult)
    <=> (x+x) + 2 * (soma t1 + soma t2)                     (distro mult)
    <=> 2 * (x + soma t1 + soma t2)                         (s2)
    <=> 2 * soma (No x t1 t2)
    
    c.q.d
 -}