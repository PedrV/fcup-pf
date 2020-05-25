--- Resoluçao do Teste Intercalar de 2018/2019 ---

-- 1
--a) [[1,2],[],[3,4],[5]] 
--b) [5]
--c) 2
--d) [16..32]
--e) [(3,2), (4,3), (5,4), (5,6), (6,8), (7,12)]
--f) [[2,8],[4,6],[]]
--g) [x <- zip [0..6] [6,5..0], x + y == 6]
--h) 15
--i) [(Char, String)]
--j) f :: (Ord a, Num a) => a -> [a] -> Bool
--k) ig :: Eq a => [a] -> Bool
--l) fix :: Eq a => (a -> a) -> a -> Bool

--2 
--a)
-- Afinal nem sequer é preciso verificar que é triangulo, e é garantido que ints sao positivos
pitagoricos :: (Ord a, Num a) => a -> a -> a -> Bool
pitagoricos a b c | a >= b && a >= c      = (a*a) == (b*b) + (c*c)
                  | b >= a && b >= c      = (b*b) == (c*c) + (a*a)
                  | c >= a && c >= b      = (c*c) == (b*b) + (a*a)

--b)
hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt((a*a) + (b*b))


--3
--a)
diferentes :: Eq a => [a] -> [a]
diferentes [] = []
diferentes [x] = []
-- diferentes [x] = [x] ultimo elem é sempre eliminado
diferentes (x:y:xs) | x /= y    = x : diferentes (y:xs)
                    | otherwise = diferentes (y:xs)

--b)
diferentes' :: Eq a => [a] -> [a]
diferentes' xs = [ fst y | y <- zip xs (tail xs), (fst y) /= (snd y)] -- ++ [last xs]  ultimo ele é eliminado


--4
zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' xs ys zs = [(fst (fst l), snd (fst l), snd l) | l<- zip (zip xs ys) zs]

--5
partir :: Eq a => [a] -> a -> ([a],[a])
partir [] _ = ([],[])
partir (x:xs) n | x == n    = ([], x:xs)
                | otherwise = ((x : fst (partir xs n)), snd (partir xs n))


-- Incompleto
--6 
parts :: [a] -> [[[a]]]
parts [] = [[]]
parts (x:xs) = [[x]:ps | ps<-pss] ++ [(x:p):ps | (p:ps)<-pss]
            where pss = parts xs


-- ou usar isto em vez das linhas em recursao
-- parts xs = [ (take n xs):ps  | n<-[1..length xs], ps<-parts(drop n xs) ]
