--22
squareSum = sum [ x*x | x<-[1..100] ]

--23
aprox :: Int -> Double
aprox n = 4 * sum [ fromIntegral ((-1)^x) / fromIntegral (2*x+1) | x<-[0..n] ]

aprox' :: Int -> Double
aprox' n = sqrt ((sum [fromIntegral ((-1)^x) / fromIntegral (x+1)^2 | x<-[0..n]]) * 12)

--24
divprop :: Int -> [Int] 
divprop n = [ x | x<-[1..(n-1)], n `mod` x == 0]

--25
perfeitos :: Int -> [Int]
perfeitos n = [ x | x<-[1..n], sum (divprop x) == x ]

--26
primo :: Int -> Bool
primo n | length x == 1     = True
        | otherwise         = False
        where x = divprop n

prime = filterPrime [2..]
        where filterPrime (p:xs) = p : [x | x<-xs, x `mod` p /= 0]


--27
nextPascal :: [Int] -> [Int]
nextPascal xs = [1] ++ zipWith (+) xs (tail xs) ++ [1]

pascal = [[1]] ++ map nextPascal pascal

--28
dotprod :: [Float] -> [Float] -> Float
dotprod xs ys = sum [fst xy * snd xy | xy<- zip xs ys]

--29
pitagoricos :: Int -> [(Int, Int, Int)]
pitagoricos n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2 == z^2 {-|| y^2+z^2 == x^2 || x^2+z^2 == y^2-}]

--30
forte :: String -> Bool
forte xs = l >= 8 && ma && d && mi
        where l = length xs
              mi = or [ x >= 'a' && x <= 'z' | x<-xs]
              ma = or [ x >= 'A' && x <= 'Z' | x<-xs]
              d = or [ x >= '0' && x <= '9' | x<-xs]

