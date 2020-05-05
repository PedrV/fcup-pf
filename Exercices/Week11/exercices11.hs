import Stack

-- 78

--a)
data Shape = Rectangle Float Float | Circle Float deriving (Eq, Show, Ord)

--b)
perimetro :: Shape -> Float
perimetro (Rectangle a b) = a + a + b + b
perimetro (Circle a) = 3.14 * 2 * a 

-- 79

-- helper --

consRec :: Float -> Float -> Float -> Float -> PShape
consRec x1 y1 x2 y2 = Rectangulo (Point x1 y1) (Point x2 y2)


--1)
data Point = Point Float Float deriving (Show, Ord, Eq)
data PShape = Rectangulo Point Point deriving (Show, Ord, Eq)

--2)
area :: PShape -> Float
area (Rectangulo (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) * abs (y1 - y2)

--3)
inter :: PShape -> PShape -> Bool
inter (Rectangulo (Point x1 y1) (Point x2 y2)) (Rectangulo (Point x3 y3) (Point x4 y4)) | x3 >= x1 && y4 >= y1                  = True
                                                                                        | x3 <= x1 && y4 >= y1 && x4 >= x1      = True
                                                                                        | otherwise                             = False  
--80

--- Test functions ---
mkS :: [a] -> Stack a
mkS xs = foldr push empty xs

parentStack :: String ->  Stack Char
parentStack [] = empty    
parentStack (ss) | l == '(' || l == '[' || l == '{'   = push (l) (parentStack $ init ss) 
                 | otherwise                          = parentStack $ init ss 
                 where l = last ss
---      //      ---

parentOp :: String -> Stack Char -> Bool
parentOp [] _ = True
parentOp (s:ss) stk | s == ')'        = case top stk of
                                      '('       -> parentOp ss (pop stk)
                                      otherwise -> False

                    | s == '('        = parentOp ss (push s stk)

                    | s == ']'        = case top stk of
                                      '['       -> parentOp ss (pop stk)
                                      otherwise -> False
  
                    | s == '['        = parentOp ss (push s stk)
                    
                    | s == '}'        = case top stk of
                                      '{'       -> parentOp ss (pop stk)
                                      otherwise -> False
             
                    | s == '{'        = parentOp ss (push s stk)
                    
                    | otherwise       = parentOp ss stk

parent :: String -> Bool
parent s = parentOp s empty


--81

--a,b)

cal :: Stack Float -> String -> Float
cal stk [] = top stk
cal stk (ss)   | s == "+"         = cal (push ((top stk) + (top (pop stk))) (pop $ pop stk)) (unwords strs)
               | s == "-"         = cal (push ((top stk) - (top (pop stk))) (pop $ pop stk)) (unwords strs)
               | s == "*"         = cal (push ((top stk) * (top (pop stk))) (pop $ pop stk)) (unwords strs)
               | s == "/"         = cal (push ((top stk) / (top (pop stk))) (pop $ pop stk)) (unwords strs)
               | otherwise        = cal (push (read s) stk) (unwords strs)
               where s:strs = words ss

--c)

main = do
    expr <- getLine
    if (expr == "DONE") then
        return ()
        else 
            do
                putStrLn $ show (cal empty expr)
                main
    return()


--82

--1)

data Graph a = Vertice a [a] (Graph a) | Empty deriving Show

-- Cada vertice tem uma lista dos outros vertice a que se conecta --
constructGraph :: Eq a => [(a,a)] -> Graph a
constructGraph [] =  Empty
constructGraph (x:xs) = Vertice (fst x) ((snd x) : (map (snd) list )) (constructGraph reveList)
                    where list = (filter (\a -> (==) (fst a) (fst x)) xs)
                          reveList = (filter (\a -> (/=) (fst a) (fst x)) xs)


--2)

graph :: [(Int,Int)]
graph = [(1,2),(1,4),(2,5),(2,3),(2,6),(3,6),(6,5),(4,5)]


--3)

{- allPaths :: a -> a -> Graph a -> Int
allPaths x y (Vertice v [n]) | x == v -}


{--83

(2) - Se s = empty
    top (push x s) = top (Stack x) = x  --- Correto
    
    - Se s != empty
    top (push x s) = top (Stack (x:[s])) = x --- Correto

(3) 1ª def: isEmpty empty = isEmpty (Stack []) = legth [] == 0 = True --- Correto
    2ª def: isEmpty empty = isEmpty (Stack []) = True --- Correto

(4) - Se s = empty
    1ª def: isEmpty (push x s) = isEmpty (push x empty) = isEmpty (Stack [x]) = length [x] == 0 = False -- Correto 
    2ª def: isEmpty (push x s) = isEmpty (push x empty) = isEmpty (Stack [x]) = False --- Correto
    - Se s != empty
    1ª def: isEmpty (push x s) = isEmpty (Stack (x:[s])) = length x:[s] == 0 = False --- Correto  
    2ª def: isEmpty (push x s) = isEmpty (Stack (x:[s])) = False --- Correto  
-}


--84

data Map a b =  Map Char Int deriving Show

instance {-(Eq a, Eq b) =>-} Eq (Map a b) where
    (Map a b) == (Map c d) = a == c     

instance Num (Map a b) where
    (Map a b) + (Map d c) = Map a (b+c)
    (Map a b) * (Map d c) = Map a (b*c)

contChar :: String -> [Map Char Int]
contChar ss = foldr (\a b -> (Map a 1) : b) [] ss
 

-- WIP wtf ?? --
contCharOP :: String -> [Map Char Int]
contCharOP (s:ss) = foldl (\b a -> (head (map (\z -> if ((==) z (Map a 1)) then z + (Map '+' 1) else z) b)) : b ) [Map s 1] ss


--- Used to test the inter function with file inputs ---
{- main = do
    cont <- getLine
    let c = map read (words cont) :: [Float]
    if (cont /= "done") then
        do
            putStrLn $ show $ inter (consRec ((!!) c 0) ((!!) c 1) ((!!) c 2) ((!!) c 3)) (consRec ((!!) c 4) ((!!) c 5) ((!!) c 6) ((!!) c 7))
            main
        else 
            putStrLn "done"
    return () -}