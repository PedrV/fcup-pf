inc x = x+1 

dobro x = x + x 

quadrado x = x * x

media x y = (x+y)/2

--2
triangulo :: Int -> Int -> Int -> Bool
triangulo x y z | x >= y+z  = False
                | y >= z+x  = False
                | z >= x+y  = False
                | otherwise = True
--3
area :: Float -> Float -> Float -> Float
area x y z = sqrt(s*(s-x)*(s-y)*(s-z))
            where s = (z+x+y)/2

--4
split :: [a] -> ([a],[a])
split xs = (take l xs, drop l xs)
        where l = length xs `div` 2

--5
last' :: [a] -> a
last' xs = head $ drop ((length xs) - 1) xs

init' :: [a] -> [a]
init' xs = reverse $ tail $ reverse xs

--6
bino :: Float -> Float -> Float
bino n k = product [1..n] / (product ([1..k] ++ [1..(n-k)]))

--7
max3 :: Ord a => a -> a -> a -> a
max3 a b c  | a >= c && a >= b   = a
            | c >= a && c >= a   = c
            | otherwise          = b

max3' :: Ord a => a -> a -> a -> a
max3' a b c = max c (max a b)

--8
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (a,b,c) = (x,y,z)
                    where x = min a (min b c)
                          y = min a (max b c)
                          z = max a (max b c)

--9 
classifica :: Int -> String
classifica x | x <= 9        = "Reprovado"
             | x <= 12       = "Suficiente"
             | x <= 15       = "Bom"
             | x <= 18       = "Muito Bom"
             | otherwise     = "Muito Bom Com Distincao"

--10
xor :: Bool -> Bool -> Bool
xor p1 p2 | p1 /= p2    = True
          | otherwise   = False

--11
safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

--12
curta :: [a] -> Bool
curta xs | length xs <= 2   = True
         | otherwise        = False

