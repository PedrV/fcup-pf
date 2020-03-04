{- Haskell continuation -}

--22

sumSq :: (Enum a, Num a) => a -> a -> a
sumSq x y = sum [z^2 | z<-[x..y]]

--23
--a) 

aprox :: (Fractional a, Integral b) => b -> a
aprox n = 4 * sum [ fromIntegral ((-1)^x) / fromIntegral ((2*x+1)) | x<-[0,1..n]]

--b)

-- funcao sqrt PEDE está desenhada para trabalhar com Floating
-- extra: implementar sqrt com fractional e testar novamente
aprox' :: (Floating a, Integral b) => b -> a
aprox' n = sqrt (sum [ fromIntegral ((-1)^x) / fromIntegral ((x+1)^2) | x<-[0,1..n]]) * 12

--24
divs :: Integral  a => a -> [a]
divs n = [ x | x<-[1..n], mod n x == 0, x<n]

--25
perfection :: Integral a => a -> [Char]
perfection n = if n == y then
                "Perfection! " ++ "Soma divisores: " ++ show y ++ " = " ++ show xs
                else "Not perfection! " ++ "Soma divisores: " ++ show y ++ " = " ++ show xs
               where xs = [ x | x<-[2..n], mod n x == 0, x<n]
                     y = sum xs

--26
divsOnFire :: Integral a => a -> [a]
divsOnFire n = [ x | x<-[1..n], mod n x == 0, x<=n, x>1] 

primes :: (Integral a) => a -> a -> [a]
primes x y = [ x1 | x1<-[x..y], length (divsOnFire x1) <= 1 &&  length (divsOnFire x1) > 0 ]

primes' :: (Integral a) => a -> [a]
primes' n = [ x | x<-[1..n], mod n x == 0, x<=n]

isPrime :: (Integral a) => a -> Bool
isPrime n = let l = length (primes' n) in 
                if l <= 2 && l > 1 then True else False

--27

binom :: (Enum a, Fractional a) => a -> a -> a
binom n k = product [1..n] / (product [1..k] * product [1..(n-k)])

linha :: (Enum a, Fractional a) => a -> [a]
linha n = [binom n k | k<-[n, n-1..0]] ++ [binom n k | k<-[n, n-1..0]]

--pascal calls linha function to create the line for the given x
pascal :: (Enum a, Fractional a) => a -> [[a]]
pascal n = [linha x | x<-[0..n]]

--28

dotProd :: [Float] -> [Float] -> Float
dotProd xs ys = sum [x*y | (x,y)<-zip xs ys]

--29
pitagoricos :: Int -> [(Int,Int,Int)]
pitagoricos n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2 == z^2]

--30

anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue (x:xs) | x == True  = True
               | otherwise  = anyTrue xs 

forte :: String -> Bool
forte ss = anyTrue [x >= 'a' && x <= 'z' | x<-ss] && anyTrue [x >= 'A' && x <= 'Z' | x<-ss] && length ss > 8