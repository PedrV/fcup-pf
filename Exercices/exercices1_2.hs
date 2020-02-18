-- 1
incr :: Num a => a -> a
incr x = x + 1

quadrado :: Num a => a -> a
quadrado x = x*x

dobro :: Num a => a -> a
dobro a = a+a

media :: Float -> Float -> Float
media x y = (x+y)/2

{-
a) 26
b) 36
c) 6.0
-}

-- 2

triangulo :: Int -> Int -> Int -> Bool
triangulo x y z = 
    if x < y + z
        then True
        else if y < x + z
            then True
            else if z < y + x
                then True
                else False

-- Guardas
neatTriangulo :: Int -> Int -> Int -> Bool
neatTriangulo x y z
        | x < y + z = True
        | y < x + z = True
        | z < y + x = True 
        | otherwise = False


-- 3

area :: Float -> Float -> Float -> Float
area x y z = 
    sqrt (s*(s-x)*(s-y)*(s-z))
    where s = (x + y + z)/2

-- 4

metade :: [a] -> ([a],[a])
metade xs = (take len xs, drop len xs)
            where len = div (fromIntegral(length xs)) 2 

-- 5
--a)

last1 :: [a] -> a
last1 [] = error "empty list"
last1 xs = xs !! l
        where l = (-) (length xs) 1 -- get the last index

last2 :: [a] -> a
last2 [] = error "empty list"
last2 [x] = x
last2 (_:xs) = last2 xs -- tornar a lista cada vez mais pequena ate sobrar so 1 que é o ultimo

-- b)

init1 :: [a] -> [a]
init1 [] = error "empty list"
init1 xs = take l xs
        where l = (-) (length xs) 1 -- get the last index

init2 :: [a] -> [a]
init2 [] = error "empty list"
init2 [x] = []
init2 (x:xs) = x : init2 xs 

<<<<<<< HEAD
-- 7
--a)

max3 :: (Num a, Ord a) => a -> a -> a -> a
max3 x y z =
    if x >= y && x >= z 
        then x
        else if y >= x && y >= z
            then y
            else z

min3 :: (Num a, Ord a) => a -> a -> a -> a
min3 x y z =
    if x <= y && x <= z 
        then x
        else if y <= x && y <= z
            then y
            else z

--b)

max3' :: (Num a, Ord a) => a -> a -> a -> a
max3' x y z = max t z
              where t = max x y 

min3' :: (Num a, Ord a) => a -> a -> a -> a
min3' x y z = min t z
              where t = min x y 

--8
--a)

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b = let t = if a == b
                            then 2
                            else 1
                in
                (max a b, t)

--b)
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x,y,z) = (a,b,c)
                      where a = min3 x y z
                            c = max3 x y z
                            b = x + y + z - a - c -- buscar o do meio, valor medio de
                                                  -- 3 valores é sempre soma dos 3 (-) max e min

--9
classifica :: Int -> String
classifica x = 
            if x <= 9
                then "Reprovado"
                else if x >= 10 && x <= 12
                    then "Suficiente"
                    else if x >= 13 && x <= 15
                        then "Bom"
                        else if x >= 16 && x <= 18
                            then "Muito Bom"
                            else if x >= 19 && x <= 20
                                then "Muito Bom com Distincao"
                                else "Invalido"

classifica' :: Int -> String
classifica' x
            | x <= 9             = "Reprovado"
            | x >= 10 && x <= 12 = "Suficiente"
            | x >= 13 && x <= 15 = "Bom"
            | x >= 16 && x <= 18 = "Muito Bom"
            | x >= 19 && x <= 20 = "Muito Bom com Distincao"
classifica' _ = "Invalido BUT... YOU JUST ACTIVATED MY TRAP CARD! So you pass"

--10

xor :: Bool -> Bool -> Bool
xor x y
    | x /= y = True
    | otherwise = False

--11

safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

-- apendix do 11 (basicamente apeteceu-me) --

{- Implementacao da funcao do preludio map -}
mapImplemented :: (Int -> Int) -> [Int] -> [Int]
mapImplemented f [] = []
mapImplemented f (x:xs) = f x : mapImplemented f xs

--12
--a)

curta :: [a] -> Bool
curta xs = if length xs <= 2 then True else False

--b)

--VIP
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = (+1) (length' xs)

curta' :: [a] -> Bool
curta' xs 
        | (length' xs) < 3   = True
        | otherwise          = False

{-
curta'' :: [a] -> Bool
curta'' [] = ????
curta'' (x:xs) = let l = (+1) (length' xs) in
                     if l < 3 then True else False              
-}

--13

oneTo10 :: Int -> String
oneTo10 x = case x of
             0      -> "zero"
             1      -> "um"
             2      -> "dois"
             3      -> "tres"
             4      -> "quatro"
             5      -> "cinco"
             6      -> "seis"
             7      -> "sete"
             8      -> "oito"
             9      -> "nove"
             10     -> "dez"
             _      -> "Devo ter chamado a funcao errada"

elevenTo19 :: Int -> String
elevenTo19 x
        | x <= 10       = oneTo10 x -- 101 handeler (basicamente culpa o gajo anterior)
        | x == 11      = "onze"
        | x == 12      = "doze"
        | x == 13      = "treze"
        | x == 14      = "quatorze"
        | x == 15      = "quinze"
        | x >= 16 && x <= 19    = "deza" ++ oneTo10 (x-10)
        | otherwise    = "Mais uma vez a funcao nao deve ser esta"

twentyTo99 :: Int -> String
twentyTo99 x
        | x <= 19   = elevenTo19 x -- 101 handeler (basicamente culpa o gajo anterior)
        | x == 20   = "vinte"
        | x == 30   = "trinta"
        | x == 40   = "quarenta"
        | x == 50   = "cinquenta"
        | x == 70   = "setenta"
        | x == 80   = "oitenta"
        | x == 90   = "noventa"
        | x <= 29   = "vinte e " ++ oneTo10 (x-20)
        | x <= 39   = "trinta e " ++ oneTo10 (x-30)
        | x <= 49   = "quarenta e " ++ oneTo10 (x-40)
        | x <= 59   = "cinquenta e " ++ oneTo10 (x-50)
        | x <= 69   = "sessenta e " ++ oneTo10 (x-60)
        | x <= 79   = "sententa e " ++ oneTo10 (x-70)
        | x <= 89   = "oitenta e " ++ oneTo10 (x-80)
        | x <= 99   = "noventa e " ++ oneTo10 (x-90)
        | otherwise = "A funcao nao deve ser esta" 

ninty9To1000 :: Int -> String
ninty9To1000 x
        | x <= 99    = twentyTo99 x -- 101 handeler (basicamente culpa o gajo anterior)
        | x == 100   = "cem"
        | x == 200   = "duzentos"
        | x == 300   = "trezentos"
        | x == 400   = "quatrocentos"
        | x == 500   = "quinhentos"
        | x == 600   = "seiscentos"
        | x == 700   = "setecentos"
        | x == 800   = "oitocentos"
        | x == 900   = "novecentos" 
        | x == 1000  = "mil"    
        | x <= 199  = "cento e " ++ twentyTo99 (x-100)
        | x <= 299  = "duzentos e " ++ twentyTo99 (x-200)
        | x <= 399  = "trezentos e " ++ twentyTo99 (x-300)
        | x <= 499  = "quatrocentos e " ++ twentyTo99 (x-400)
        | x <= 599  = "quinhentos e " ++ twentyTo99 (x-500)
        | x <= 699  = "seiscentos e " ++ twentyTo99 (x-600)
        | x <= 799  = "setecentos e " ++ twentyTo99 (x-700)
        | x <= 899  = "oitocentos e " ++ twentyTo99 (x-800)
        | x <= 999  = "novecentos e " ++ twentyTo99 (x-900)
        | otherwise = "funcao errada"

mil_e_1To10000 :: Int -> String
mil_e_1To10000 x
        | x <= 99   = ninty9To1000 x -- 101 handeler (basicamente culpa o gajo anterior)
        -- TODO : Talvez haja maneira rapida de fazer os 1000, 2000 ... tentar descobrir
        | x == 2000   = "dois mil" 
        | x == 3000   = "tres mil"
        | x == 4000   = "quatro mil"
        | x == 5000   = "cinco mil"
        | x == 6000   = "seis mil"
        | x == 7000   = "sete mil"
        | x == 8000   = "oito mil"
        | x == 9000   = "nove mil" 
        | x == 10000  = "dez mil"    
        | x <= 1999  = "mil e " ++ ninty9To1000 (x-1000)
        | x <= 2999  = "dois mil e " ++ ninty9To1000 (x-2000)
        | x <= 3999  = "tres mil e " ++ ninty9To1000 (x-3000)
        | x <= 4999  = "quatro mil e " ++ ninty9To1000 (x-4000)
        | x <= 5999  = "cinco mil e " ++ ninty9To1000 (x-5000)
        | x <= 6999  = "seis mil e " ++ ninty9To1000 (x-6000)
        | x <= 7999  = "sete mil e " ++ ninty9To1000 (x-7000)
        | x <= 8999  = "oito mil e " ++ ninty9To1000 (x-8000)
        | x <= 9999  = "nove mil e " ++ ninty9To1000 (x-9000)
        | otherwise = "neste ponto so RIP"

dez_1To100k :: Int -> String
dez_1To100k x
        -- TODO : Talvez haja maneira rapida de fazer os 10000, 20000 ... tentar descobrir
        | x == 20000   = "vinte mil" 
        | x == 30000   = "trinta mil"
        | x == 40000   = "quarenta mil"
        | x == 50000   = "cinquenta mil"
        | x == 60000   = "sessenta mil"
        | x == 70000   = "setenta mil"
        | x == 80000   = "oitenta mil"
        | x == 90000   = "noventa mil" 
        | x == 100000  = "cem"    
        | x >= 10001 && x <= 19999   = elevenTo19(div x 1000) ++ " " ++ mil_e_1To10000 (x-10000)
        | x <= 29999  = "vinte " ++ mil_e_1To10000 (x-20000)
        | x <= 39999  = "trinta " ++ mil_e_1To10000 (x-30000)
        | x <= 49999 = "quarenta " ++ mil_e_1To10000 (x-40000)
        | x <= 59999  = "cinquenta " ++ mil_e_1To10000 (x-50000)
        | x <= 69999  = "sessenta " ++ mil_e_1To10000 (x-60000)
        | x <= 79999  = "setenta " ++ mil_e_1To10000 (x-70000)
        | x <= 89999  = "ointenta " ++ mil_e_1To10000 (x-80000)
        | x <= 99999  = "noventa " ++ mil_e_1To10000 (x-90000)
        | otherwise = "never give up"

-- textual :: Int -> String


{-Extra-}
-- dif a começar da esquerda para a direita
diff :: [Int] -> Int
diff (x:xs) = foldl (-) x xs

-- dif a começar da direita para esquerda
diff' :: [Int] -> Int
diff' (x:xs) = foldr (-) x xs

times4 :: Int -> Int
times4 x = x*4

listTimes4 = map times4 [1..10]

{- lambda forma apenas funcoes temporarias que sao usadas naquele contexto -}
lambda = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
=======
-- 6
--a)

binom :: (Fractional a, Integral b ) => b -> b -> a
binom n k = fromIntegral(nFac) / fromIntegral((kFac * nkFac))
            where nFac = product [1..n]
                  kFac = product [1..k]
                  nkFac = product [1..n-k]

--b)

binom' :: (Fractional a) => Int -> Int -> a
binom' n k = fromIntegral(nFac) / fromIntegral(kFac)
            where nFac = product (drop (n-k) [1..n])
                  kFac = product [1..k]
>>>>>>> 9ec850b2d87255ada7f7e0876fd7f918eb6a2c13
