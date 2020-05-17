--- Resoluçao do Teste Intercalar de 2018/2019 ---

-- 1
--a) [[1,2]],[],[3,4],[5]] 
--b) [5]
--c) 2
--d) [4..32]
--e) [(3,2), (4,3), (5,4), (5,6), (6,8), (7,12)]
--f) [[2,8],[4,6],[]]
--h) 15
--i) [(Char, String)]
--j) f :: (Num a) -> a -> [a] -> Bool
--k) ig :: Eq a => [a] -> Bool
--l) fix :: Eq a => (a -> a) -> a -> Bool

--2 
--a)
pitagoricos :: (Ord a, Num a) => a -> a -> a -> Bool
pitagoricos a b c | a >= b && a >= c      = (a*a) == (b*b) + (c*c)
                  | b >= a && b >= c      = (b*b) == (c*c) + (a*a)
                  | c >= a && c >= b      = (c*c) == (b*b) + (a*a)
                  | otherwise           = error "pitagoricos: Not a triangle (rectangle)"

--b)
hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt((a*a) + (b*b))


--3
--a)
diferentes :: Eq a => [a] -> [a]
diferentes [] = []
diferentes [x] = [x]
diferentes (x:y:xs) | x /= y    = x : diferentes (y:xs)
                    | otherwise = diferentes (y:xs)

--b)
diferentes' :: Eq a => [a] -> [a]
diferentes' xs = [ fst y | y <- zip xs (tail xs), (fst y) /= (snd y)] ++ [last xs]


--4
zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' xs ys zs = [(fst (fst l), fst (snd l), snd l) | l<- zip (zip xs ys) zs]

--5
partir :: Eq a => [a] -> a -> ([a],[a])
partir [] _ = ([],[])
partir (x:xs) n | x == n    = ([],[])
                | otherwise = ((x : fst (partir xs n)), (dropWhile (/= n) xs))

-- Incompleto
--6 
parts :: [a] -> [[a]]
parts xs = [ [l,l1] | l<-xs, l1<- xs, l /= l1] ++ [xs]
