import Data.List

-- Exame Ano Letivo 18/19 Epoca Recurso --
-- https://bit.ly/2ZGojcC

--1
--a) [[],[]] ++ [[],[]] = [[],[],[],[]]
--b) 4
--c) [0,0,0,1,1,1,1,2,2,2]
--d) [1,3,9,27,81]
--e) [4,8]
--f) [2,4,6,8,10,12,14,16,18,20]
--g) [ if even x then 2*x else 2*x - 1 | x <- [0..] ]
--h) False
--i) [(Int -> Bool)]
--j) [Char] -> Int
--k) data Arv = No Arv Int Arv | Vazia deriving Eq
--l) (a -> b -> c) -> b -> a -> c 



--2

imparDiv3 :: [Int] -> Bool
imparDiv3 xs = and [odd x | x<-xs , x `mod` 3 == 0] 

imparDiv3' :: [Int] -> Bool
imparDiv3' xs =  all (\x -> odd x) (filter (\x -> x `mod` 3 == 0) xs)


--3 

type Rel a = [(a,[a])]
--composta :: Rel a -> Rel a -> Rel a
--composta r1 r2 = [(x, [ nub . concat ay | y <- ax, (y', ay) <- r2, y == y']) | (x,ax) <- r1]


-- VIP
--4 
list = 1 :  [ 2*x+y+1  | (x,y) <- zip list [2..]]

--5
duplicada :: Eq a => [a] -> Bool
duplicada [] = True
duplicada [x] = False
duplicada (x:y:xs) | x == y     = duplicada xs
                   | otherwise  = False


duplica :: [a] -> [a]
duplica xs = concat [ x : [x] | x<-xs]
--ou 
-- duplica xs =  [x | x<-xs, _ <- [1,2]]
-- duplica xs =  [x | x<-xs, _ <- [x,x]]


--6
data Arv a = Folha a | No (Arv a) (Arv a)

--a) 
emOrdem :: Arv a -> [a]
emOrdem (Folha a) = [a]
emOrdem (No esq dir) = emOrdem esq ++ emOrdem dir  

--b) 
anyArv :: (a -> Bool) -> Arv a -> Bool
anyArv f (Folha a) = f a
anyArv f (No esq dir) = anyArv f esq || anyArv f dir


--7
{- 
emOrdem (Folha a) = [a]  -- o1
emOrdem (No esq dir) = emOrdem esq ++ emOrdem dir -- o2 

anyArv f (Folha a) = f a    -- av1
anyArv f (No esq dir) = anyArv f esq || anyArv f dir    -- av2

any (xs++ys) = any xs || any ys -- a1


Objetivo: anyArv p t = any p (emOrdem t)

Caso Base: 

        anyArv p (Folha a)          -- av1
        p a                         -- Elem neutro de ||
        p a || False                -- any em []
        p a || p []                 -- a1
        any p ([a])                 -- o1
        any p (emOrdem (Folha a))

Induçao:  -- Supondo que isto se mantém entre 0 e n, isto é verdade para n então é pata n+1 --

Hipotese de Indução: 

            anyArv p t = any p (emOrdem t) => anyArv p (No esq dir) = any p (emOrdem (No esq dir))

            anyArv p (No esq dir)                           -- av2
            anyArv p esq || anyArv p dir                    -- H.I
            any p (emOrdem esq) || any p (emOrdem dir)      -- a1 
            any p (emOrdem esq ++ emOrdem dir)              -- o2
            any p (emOrdem (No esq dir))

            c.q.d 
-}
