{- Week 0: trying stuff before actually learn something -}

sum1 :: (Int -> (Int -> Int))
sum1 x y = x+y -- recreating the sum function

def1 :: Int -> Int -> Int 
def1 x y = x-y -- recreating the diference function

mult1 :: Int -> Int -> Int
mult1 x y = x*y -- recreating the multiplication function

media :: [Float] -> Float
media xs =  sum xs / fromIntegral (length xs) 
-- de modo a prevenir erro de tipo, aplica-se
-- explicit cast com a funcao fromIntegral que converte qualquer tipo inteiro
-- para qualquer tipo numérico


swap :: (a,b) -> (b,a) 
swap (x,y) = (y,x)

{- curried
swap :: a -> b -> (b,a)
swap x y = (y,x)
-}

pair :: a -> b -> (a,b) 
pair x y = (x,y)

-- Passar uma funcao a uma funca com (a -> a)
-- mais um argumento x representado pelo parametro a : (a -> a) -> a
-- depois retornar um a: (a -> a) -> a -> a
times2 :: Num a => a -> a
times2 a = 2*a

twice :: (a -> a) -> a -> a
twice f x = f (f x)

fourFtimes :: (a -> a) -> a -> a
fourFtimes f x = f (f (f (f x)))

zip' :: [a] -> [(a,a)]
zip' xs = zip xs xs

sumOf3 :: Num a => a -> a -> a -> a
sumOf3 x y z = x+y+z

many3 = replicate 10 3

listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]
-- Lista em que pegamos nos valores da lista de 1 a 10,
-- multiplicamos por 3 cada um destes valores
-- e depois filtramos apenas os valores que sao <= 50

zero :: (Eq a, Num a) => a -> Bool
zero x = if x == 0 then True 
         else False


min3 :: (Int -> (Int -> (Int -> Int)))
min3 x y z =
    if x < y && x < z 
        then x
        else if y < x && y < z
            then y
            else z

{- Com guardas -}
neatMin3 :: (Ord a ,Num a) => a -> a -> a -> a
neatMin3 x y z
    | x < y && x < z = x
    | y < x && y < z = y
    | otherwise      = z 

data Colour = Black | White | RGB Int Int Int

describeBlackOrWhite :: Colour -> String
describeBlackOrWhite c =
  "This colour is"
  ++ case c of
       Black           -> " black"
       White           -> " white"
       RGB 0 0 0       -> " black"
       RGB 255 255 255 -> " white"
       _               -> "... uh... something else"
  ++ ", yeah?"

iguais3 :: (Eq a, Num a) => a -> a -> a -> Bool
iguais3 x y z =
    if z == x && z == y
        then True
        else False

media1 :: Fractional a => a -> a -> a
media1 x y = (x + y) / 2

par :: Integral a => a -> Bool
par x = 
    if x `mod` 2 == 0
        then True
        else False

sinal :: (Ord a, Num a) => a -> a
sinal x =
    if x < 0
        then -1
        else if x == 0
            then 0
            else 1

divisibleBy2 = [ x | x <- [1..10], x `mod` 2 == 0]

factorial :: Int -> Int
factorial n = product [1..n]

whatGrade :: (Ord a, Num a) => a -> String
whatGrade age
    | (age >= 5) && (age <= 6 ) = "Kindergarten"
    | (age > 6) && (age <= 10 ) = "Elementary School"
    | (age > 10) && (age <= 14 ) = "Middle School"
    | (age > 14) && (age <= 18 ) = "High School"

whatGrade _ = "You just activated my trap card!" 

getListItens :: [Int] -> String
getListItens [] = "The list is empty"
getListItens (x:[]) = "The first item of the list is " ++ show x
getListItens (x:y:[]) = "The second item of the list is " ++ show y
getListItens (x:xs) = "The first item of the list is " ++ show x ++ " and the rest is " ++ show xs

getTheList :: String -> String
getTheList [] = "No words"
getTheList all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

main :: IO ()
main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")
