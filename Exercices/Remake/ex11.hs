import Stack

--78
--a)
data Shape = Rectangle Float Float | Circle Float deriving (Eq,Ord)

instance Show Shape where
    show (Rectangle f1 f2) = "Rectangle" ++ show f1 ++ show f2
    show (Circle r) = "Circle" ++ show r

--b)
perimetro :: Shape -> Float
perimetro (Circle r) = 3.14 * (2*r)
perimetro (Rectangle f1 f2) = 2 * f1 + 2 * f2

--79
data Retangulo = R (Int,Int) (Int, Int) deriving (Eq,Ord,Show)

area :: Retangulo -> Int
area (R (x1,y1) (x2,y2)) = (x2-x1) * (y2-y1)

inter :: Retangulo -> Retangulo -> Bool
inter (R (x1,y1) (x2,y2)) (R (x3,y3) (x4,y4)) | x3 > x2 || x4 < x1   = False
                                              | y3 > y2 || y4 < y1   = False
                                              | otherwise            = True  

--80

parent' :: String -> Bool
parent' xs = parent xs empty


parent :: String -> Stack Char -> Bool
parent "" s = isEmpty s
parent (x:xs) s | x == '('        = parent xs (push x s)
                | x == '['        = parent xs (push x s)
                | x == ')'        = if isEmpty s then 
                                    False else 
                                        if top s == '(' then 
                                            parent xs (pop s) else False
                | x == ']'        = if isEmpty s then
                                    False else
                                        if top s == '[' then 
                                            parent xs (pop s) else False
                | otherwise       = parent xs s



--81
--a)

size1 :: Stack a -> Int
size1 s | isEmpty s     = 0
        | otherwise     = 1 + size1 (pop s)

calc :: Stack Float -> [String] -> Stack Float
calc s [] = s
calc s (x:xs) | x == "*"        = if size1 s < 2 then error "calc: expressao incorreta" 
                                  else calc (push (top s * top (pop s)) (pop $ pop s)) xs
              | x == "/"        = if size1 s < 2 then error "calc: expressao incorreta" 
                                  else calc (push (top s / top (pop s)) (pop $ pop s)) xs
              | x == "-"        = if size1 s < 2 then error "calc: expressao incorreta" 
                          4        else calc (push (top s - top (pop s)) (pop $ pop s)) xs
              | x == "+"        = if size1 s < 2 then error "calc: expressao incorreta" 
                                  else calc (push (top s + top (pop s)) (pop $ pop s)) xs
              | otherwise       = calc (push (read x) s) xs

--b)
calcular :: String -> Float
calcular xs = top $ calc empty (words xs)


--c)
rpn :: IO ()
rpn = do
    string <- getLine
    if string == "Done" then
        return ()
        else do
            putStrLn (show $ calcular string)
            rpn

--82
data Graph = Graph ( [Int] , [(Int, Int)] )

grafo :: Graph
grafo = Graph ([1..6], [(1,2), (1,4), (2,3), (2,5), (2,6), (3,6), (4,5), (6,5)])

destinos :: Graph -> Int -> [Int]
destinos (Graph (v, a)) o = [snd x | x<- a, fst x == o]

caminhos :: Graph -> Int -> Int -> Int
caminhos (Graph (v,a)) o d | (o == d)   = 1
                           | otherwise  = sum [ caminhos (Graph (v,a)) x d | x <- dest  ]
                           where dest = destinos (Graph (v,a)) o



--84
{- data Map = Map Char Int

instance Eq Map where
    (==) (Map c i) (Map c1 i1) = c == c1

instance Show Map where
    show (Map c i) = show c ++ ":" ++ show i

instance Num Map where
    (+) (Map c i) (Map c1 i1) | c == c1   = Map c i+1
                              | otherwise = Map c i
    (-) (Map c i) (Map c1 i1) | c == c1   = Map c i-1
                              | otherwise = Map c i
    fromInteger 
    

contains :: [Map] -> Map -> Bool
contains [] k = False
contains (x:xs) k | k == x    = True
                  | otherwise = contains xs k

get :: [Map] -> Map -> Map
get [] m = m
get (x:xs) m | x == m       = x
             | otherwise    = get xs m


chars :: String -> [Map]
chars xs = foldr (\a b -> ((get b (Map a 0)) + (Map a 1)) : b) [] xs -}


--88
data Map k v = No (k,v) (Map k v) (Map k v) | EmptyMap deriving (Show,Ord,Eq)

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Show, Eq, Ord)


qsort :: Ord a => [(a,b)] -> [(a,b)]
qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ qsort right
              where left = [ y | y<-xs, (fst y) < (fst x) ]
                    right = [ y | y<-xs, (fst y) > (fst x) ]


emptyMap :: Map k v
emptyMap = EmptyMap

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k a EmptyMap = No (k,a) EmptyMap EmptyMap
insert k a (No (x,y) mleft mright) | k < x      = No (x,y) (insert k a mleft) mright
                                   | k > x      = No (x,y) mleft (insert k a mright)
                                   | otherwise  = No (x,y) mleft mright

mais_dir :: Map k v -> (k,v)
mais_dir (No (k,v) _ EmptyMap) = (k,v)
mais_dir (No (k,v) _ mright) = mais_dir mright


remove :: Ord k => k -> Map k v -> Map k v
remove k (No (x,y) EmptyMap EmptyMap) = if k == x then EmptyMap else No (x,y) EmptyMap EmptyMap
remove k (No (x,y) mleft mright) | k < x        = No (x,y) (remove k mleft) mright
                                 | k > x        = No (x,y) mleft (remove k mright)
                                 | otherwise    = No z mleft mright
                                 where z = mais_dir mleft

constructMap :: Ord k =>  [(k,v)] -> Map k v
constructMap [] = EmptyMap
constructMap xs = No m (constructMap left) (constructMap right)
                where m = (!!) list l 
                      left = take l list
                      r:right = drop l list
                      l = (length list) `div` 2
                      list = qsort xs 

lookup' :: Ord k => k -> Map k v -> Maybe k
lookup' _ EmptyMap = Nothing
lookup' k (No (x,y) mleft mright) | k < x        = lookup' k mleft
                                  | k > x        = lookup' k mright
                                  | otherwise    = Just x