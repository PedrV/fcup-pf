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