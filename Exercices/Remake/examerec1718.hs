-- 17.48/20

--1
a) 4
b) [5,4,3]
c) 6
d) [ (0,1),(0,3),(1,2),(1,4),(2,3),(3,4) ]
e) [1,9,25,49,81]
f) 6
g) serie = 1 : [ if y `mod` 2 /= 0  then (abs x)*2+1 else -((abs x)*2+1)| (x,y)<- zip serie [0..] ]
   serie1 = [ if y `mod` 2 == 0 then -x else x | (x,y)<- zip [2,4..] [0..]] 
   final = [ (x,y) | (x,y)<-zip serie1 serie ]
h) 7*8+3
i) [Int] -> Bool
j) (Num a, Ord a) [a -> Bool] -- errado antes
k) Folha | No (Arv) Int (Arv)
l) (Ord a, Num a) => [b] -> [b] 

--2
--a)

fst :: (a,b,c,d) -> a
fst (a,_,_,_) = a

snd :: (a,b,c,d) -> b
fst (_,b,_,_) = b

ird :: (a,b,c,d) -> c
ird (_,_,c,_) = c

fth :: (a,b,c,d) -> d
fth (_,_,_,d) = d

pontuacao :: [(String,Int,Int,Int)] -> [(String,Int)]
pontuacao xs = foldr (\a b -> (fst a, (3*(snd a) + 1*(ird a))) : b ) [] xs


--b)
njogos :: Int -> [(String,Int,Int,Int)] -> Bool
njogos n xs = and $ foldr (\a b -> if (fst a) + (snd a) + (ird a) == n then True : b else False : b) [] xs

--3

allCrescente :: [Int] -> Bool
allCrescente [] = True
allCrescente [x] = True
allCrescente (x:y:xs) | x > y       = allCrescente (y:xs)
                      | otherwise   = False    

crescente' :: [Int] -> IO ()
crescente' xs = do
    x<-getLine
    if allCrescente ((read x):xs) then do
        crescente' ((read x):xs)
        return ()
        else do
            putStrLn (show $ length xs)
            return ()

crescente :: IO ()
crescente = crescente' []


--4

maisvezes = 1 : [ if y `mod` 2 == 0 then 2+x else 2*x  | (x,y)<-zip maisvezes [0..] ]


--5
--a)
caminho :: Eq a => [(a,a)] -> Bool
caminho [] = True
caminho [x] = True
caminho (x:y:xs) | snd x == fst y       = caminho (y:xs)
                 | otherwise            = False


--b)
caminhoG :: Eq a => [(a,a)] -> [(a,a)] -> Bool
caminhoG ps as = and [ elem p as | p<-ps ]


--6

data ArvC a = Vazia | No a Int (ArvC a) (ArvC a)

--a)

nelem :: ArvC a -> Int
nelem Vazia = 0
nelem (No a _ esq dir) = 1 + (nelem esq) + (nelem dir)


--b)

update :: ArvC a -> ArvC a
update Vazia = Vazia
update (No _ x esq dir) = 



--7

--b)
 
Objetivo: map f xs = foldr (\x xs -> (f x):xs) [] xs

Caso Base: 
        map f []                                (m1)
        []                                      (f1)    
        foldr (\x xs -> (f x) : xs) [] []


Caso de Indução: 
        map f xs = foldr (\x xs -> (f x):xs) [] xs => map f (x:xs) = foldr (\x xs -> (f x):xs) [] (x:xs)

Hipotese de Indução: map f xs = foldr (\x xs -> (f x):xs) [] xs

        map f (x:xs)                                (m2)
        (f x) : map f xs                            H.I
        (f x) : foldr (\x xs -> (f x):xs) [] xs     (f2)
        foldr (\x xs -> (f x):xs) [] (x:xs) 
        



