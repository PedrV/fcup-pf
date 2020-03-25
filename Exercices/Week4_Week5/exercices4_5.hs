-- 31
import Control.Monad(guard)

pot2 :: (Eq a, Num a) => a -> a
pot2 0 = 1
pot2 n = 2 * pot2(n-1)

--32 / 33
--a)
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == False     = False 
            | otherwise      = and' xs

--b)
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)  | x == True     = True
            | otherwise     = or' xs

--c)
--list comp
concat' :: [[a]] -> [a]
concat' xss = [ x | xs<-xss, x<-xs]

--recursive
--cons operator (:) searchs for a element to be the head (2:[2,3]) -> funciona
-- (:) puts 2 on the head and the rest ([2,3]) has tail
-- on the other hand (++) just puts 2 things together no tail no head
concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (xs:xss) = xs ++ concat'' xss

--d)
--recursive
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a : replicate' (n-1) a

--list comp
replicate'' :: Int -> a -> [a]
replicate'' n a = [ a | x<-[0..n]]

--e)

--list comp
(!!!) :: [a] -> Int -> [a]
(!!!) xs n = let l = length xs
                in let ys = zip xs [1..l]
                    in [ x | (x,y)<-ys, y == n]


--recursive
-- (!!!!) :: [a] -> Int -> a
(!!!!) [] n = [] 
(!!!!) (x:xs) n | n == 0    = x
                | otherwise = (!!!!) xs n


--f)

-- recursive
elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) | y == x     = True
               | otherwise  = elem' y xs

-- list comp
anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue (x:xs) | x == True  = True
               | otherwise  = anyTrue xs 

elem'' :: Eq a => a -> [a] -> Bool
elem'' y xs = anyTrue [ x==y | x<-xs]


--34

--a)
leastSqrt1 :: (Num a, Ord a) => a -> a -> a
leastSqrt1 n k  | k * k > n     = k
                | otherwise     = leastSqrt1 n (k+1)

--b)
issqrt :: (Num a, Ord a, Enum a) => a -> a
issqrt n = (!!) [x | x<-[1..n], (x*x < n && (x+1)*(x+1) > n)] 0


--35

--a)
factorial :: (Eq a, Num a) => a -> a
factorial 1 = 1
factorial n = n * factorial (n-1)

--b)
rangeProduct :: (Num a, Enum a) => a -> a -> a
rangeProduct a b = product [ x | x<-[a..b]]

--c)
facRange :: (Num a, Enum a) => a -> a
facRange n = rangeProduct 1 n

--36
mdc :: Integer -> Integer -> Integer
mdc a b | b == 0    = a
        | otherwise = mdc b (mod a b)

--37

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : (nub [y | y<-xs, y /= x])
                -- nub de apenas os elementos que sao diferentes daquele que eu ja pos no inicio (x)

--38

interspace :: a -> [a] -> [a]
interspace b []  = []
interspace b [x] = [x]
-- ou -- interspace b (x:xs) = [x] ++ [b] ++ interspace b xs
interspace b (x:xs) = x : b : interspace b xs 

--39

t2 a = a+2  

fapl :: (Ord a, Num a) => (a -> b) -> a -> [b]
fapl f n | n > 0     = f n : fapl f (n-1)
         | otherwise = []

max' :: Ord a => [a] -> a
max' [x] = x
max' (x:xs) | (max' xs) > x    = max' xs
            | otherwise        = x

fmax :: (Num a, Ord a) => (a -> a) -> a -> a
fmax f n = max' (fapl f n)

--40

anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f n = or' [f x == 0 | x<-[n,n-1..0]]

--41

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n = sum [f x| x<-[n,n-1..0]]

--42 
--a)

insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n [x] | n >= x      = [x] ++ [n]     
              | n < x       = [n] ++ [x]
insert' n (x:y:xs) | n >= x && n <= y    = x : n : y : xs
                   | n <= x              = n : x : y : xs  
                   | otherwise           = x : y : insert' n xs

--b

isOrd :: Ord a => [a] -> [a]
isOrd [] = []
isOrd (x:xs) = insert' x (isOrd xs)


--43
--a)
--check if a list is in increasing order
isOrder :: Ord a => [a] -> Bool
isOrder [] = True
isOrder [x] = True
isOrder (x:y:xs) | x > y       = False
                 | otherwise   = isOrder (y:xs) 

minimum' :: Ord a => [a] -> a
minimum' [x] = x
minimum' (x:xs) | list == []         = x
                | otherwise          = minimum' xs               
                where list = [y | y<-xs, y <= x]
--b)

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete n (x:xs) | n == x    = xs
                | otherwise = x : delete n xs

--c)

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort (x:xs) = l : ssort ls
               where l = minimum (x:xs) 
                     ls = delete l (x:xs)

--44
--a)

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys 
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | x > y     = y : merge (x:xs) ys

--b)

qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort ll ++ [x] ++ qSort rl
               where ll = [ y | y<-xs, y<=x ]
                     rl = [ y | y<-xs, y>x ]

{-  test area
splitAt' :: [a] -> Int -> ([a],[a])
splitAt' xs n = ( (take n xs), (drop (((-) l n)-1) xs) )
            where l = length xs

accTupples :: Int -> (a,a) -> a
accTupples  n (x,y) | n == 1     = x
                    | otherwise  = y

test area -}

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (x:xs) = merge (qSort [y | y<-(x:xs), y<=x])  (qSort [y | y<-(x:xs), y>x])

--45
-- list monad
bits :: Int -> [[Bool]]
bits 0 = [[]]
bits n = do
    c <- [True,False]
    map (c :) (bits (n-1))

-- list comp
bits' :: Int ->[[Bool]]
bits' n = [ [i,j] | i<-[True, False], j<-[True,False]]

--45 (for numbers I guess)
-- list monad
comb :: Int -> Int -> [[Int]]
comb 0 _ = [[]]
comb n x = do
    c <- [1..x]
    map (c :) (comb (n-1) x)

-- list comp (for numbers)
comb' :: Int ->[[Int]]
comb' n = [ [i,j] | i<-[1..n], j<-[1..n]]


--46 

changeHead :: (Eq a, Num a) => [a] -> a -> [a]
changeHead [] _ = []
changeHead (x:xs) n | n == 0    = x : xs
                    | otherwise = changeHead xs (n-1) ++ [x]

theRealSelect :: [Int] -> Int -> Int -> [(Int,[Int])]
theRealSelect [] _ _ = []
theRealSelect (x:xs) n l | n > l-2      = [(x,xs)]
                         | otherwise    = [(x,xs)] ++  theRealSelect (changeHead (x:xs) 1) (n+1) l

select :: [Int] -> [(Int,[Int])]
select xs = theRealSelect xs 0 (length xs)
 

{- test area -}

--syntax sugar for epicMeme
epicMemeWithSugar :: Integral a => [a] -> [String] --Integral because even uses div
epicMemeWithSugar xs = [ if x < 20 then "Epic?" else "MEME!" | x<-xs , even x]

epicMeme :: Integral i => [i] -> [String]
epicMeme xs = do
    x <- xs
    guard (even x)
    return (if x < 20 then "Epic?" else "MEME!")

toty 0 = [[]] 
-- map tem de encontrar lista de algo para aplicar 
{- map ([0,2,4] :) map ([0,2] :) [[]] -}
toty n = map ([y*2 | y<-[0..n]] :) (toty (n-1)) -- syntax for create list of lists using recursion

toty1 0 = [[]]
toty1 n = ([y*2 | y<-[0..n]] :) (toty1 (n-1)) -- syntax for create list of lists using recursion

{- map quer uma lista de elementos que possa aplicar uma funcao,
   1. map (+2) [40,18]. Isto soma dois a todos os elementos da lista

   2.  map ([1] :) [[[2,4],[5,69]]]. 
   Para começar o operador (:) constroi uma lista,
   colocado o elemnto (x) há sua esquerda como cabeça do lista de elementos do tipo (x) há sua direita.

   Por exemplo [1,2,3] nada mais é que (1:2:3:[]). Posto de forma muito simples, se quanto mais [] se
   tem mais elevado é o estatuto de um elemento, então (:) há esquerda precisa de um elemento
   com UM grau de estatuto abaixo para juntar aos de estatudo acima há direita:

    (1)e.g. 1 : [1,2] -- valido, (2)e.g. [2] : [[2,3]] -- valido, e.g. [[[3]]] : [[[[4,2]]]] -- valido
    (3)e.g. 4 : 3 -- invalido, e.g. [4] : [6] -- invalido, e.g. [1] : 2 -- invalido

   Outra forma de pensar é que (:) vai juntar um elemento de uma categoria,
   com uma lista de outros elementos da mesma categoria. Por exemplo em (1) vou juntar digitos,
   a uma lista com mais digitos. Em (2) junto lista de digitos a uma lista de listas de digitos.
   Já em (3) tento, sem sucesso, juntar um digito a um digito. 

   Usando map (1 :) ([[1,2]]), o map vai procurar uma lista que contenha digitos, pois 1 é digito,
   e junta-lo a todas as listas que encontrar. Como map só atua em listas, precisamos de envolver
   os elemtos numa lista para ele procurar. Isto permite-nos colocar (1) há cabeça em varias listas
   ao mesmo tempo. Pelo contrário, 1 : [1,2], apenas poe numa lista, 1 : [[1,2],[3,4]] nao iria
   resultar pois é uma lista de listas e nao lista de digitos. Para concluir no caso de 
   map (1 : ) ([[1,2],[3,4]]), ele iria intrepretar como, ora bem vou procura nesta minha lista por 
   listas de digitos e fica algo como 1 : [1,2] e 1 : [3,4] 
    -}