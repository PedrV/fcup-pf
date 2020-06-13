data Arv a = No a (Arv a) (Arv a) | Folha deriving (Show, Eq, Ord)

--- BST ---

listar :: Arv a -> [a]
listar Folha = []
listar (No a left right) = listar left ++ [a] ++ listar right

qsortN :: Ord a => [a] -> [a]
qsortN [] = []
qsortN (x:xs) = (qsortN left) ++ [x] ++ (qsortN right)
              where left = [ y | y<-xs, y < x ]
                    right = [ y | y<-xs, y > x ]


constructTree :: Ord a => [a] -> Arv a
constructTree [] = Folha
constructTree xs = No a (constructTree left) (constructTree right)
                   where left = take l ys
                         (r:right) = drop l ys
                         a = (!!) ys l
                         l = (length ys) `div` 2 
                         ys = qsortN xs


inserir :: Ord a => a -> Arv a -> Arv a
inserir x Folha = No x Folha Folha
inserir x (No a left right) | x > a     = No a left (inserir x right)
                            | x < a     = No a (inserir x left) right
                            | otherwise = No a left right

contains :: Ord a => a -> Arv a -> Bool
contains x Folha = False
contains x (No a left right) | x > a        = contains x right
                             | x < a        = contains x left
                             | otherwise    = True

numberNodes :: Arv a -> Int
numberNodes Folha = 0
numberNodes (No a left right) = 1 + numberNodes left + numberNodes right

depth :: Arv a -> Int
depth Folha = 0
depth (No a left right) = 1 + max (depth left) (depth right) 

maisDir :: Arv a -> a
maisDir (No a left Folha) = a
maisDir (No a left right) = maisDir right 

maisEsq :: Arv a -> a
maisEsq (No a Folha right) = a
maisEsq (No a left right) = maisEsq left


remove' :: Ord a => a -> Arv a -> Arv a
remove' _ Folha = Folha
remove' x (No a Folha right) = if x == a then right else No x Folha (remove' x right)
remove' x (No a left Folha) = if x == a then left else No x (remove  x left) Folha
remove' x (No a Folha Folha) = if a == x then Folha else No a Folha Folha
remove' x (No a left right) | x < a         = No a (remove' a left) right
                            | x > a         = No a left (remove' a right)
                            | otherwise     = No y left (remove' y right)
                            where y = maisEsq right   

-- 76 b)
remove :: Ord a => a -> Arv a -> Arv a
remove _ Folha = Folha
remove x (No a Folha Folha) | x == a    = Folha
                            | otherwise = No a Folha Folha
remove x (No a Folha right) | x == a    = right
                            | otherwise = No x Folha (remove x right)
remove x (No a left Folha) | x == a     = left
                           | otherwise  = No x (remove x left) Folha
remove x (No a left right) | x < a      = No a (remove x left) right
                           | x > a      = No a left (remove x right)
                           | otherwise  = No y (remove y left) right
                           where y = maisDir left 


isBalanced :: Arv a -> Bool
isBalanced Folha = True
isBalanced (No a left right) = abs (depth left - depth right) <= 1 && isBalanced left && isBalanced right


--- AVL ---


containsAVL :: Ord a => a -> Arv a -> Bool
containsAVL x Folha = False
containsAVL x (No a left right) | x > a        = containsAVL x right
                                | x < a        = containsAVL x left
                                | otherwise    = True

-- 76 a)
maisDirAVL :: Arv a -> a
maisDirAVL (No a left Folha) = a
maisDirAVL (No a left right) = maisDir right 

maisEsqAVL :: Arv a -> a
maisEsqAVL (No a Folha right) = a
maisEsqAVL (No a left right) = maisEsq left


depthAVL :: Arv a -> Int
depthAVL Folha = 0
depthAVL (No a left right) = 1 + max (depth left) (depth right) 

desvio :: Arv a -> Int
desvio Folha = 0
desvio (No x left right) = depthAVL left - depthAVL right

rotR :: Arv a -> Arv a
rotR (No x (No y t1 t2) t3) = No y t1 (No x t2 t3)
rotR t = t

rotL :: Arv a -> Arv a
rotL (No x t1 (No y t2 t3)) = No y (No x t1 t2) t3
rotL t = t

corrLeft :: Arv a -> Arv a
corrLeft (No x left right) | d == 1    = rotL (No x left (rotR right))
                           | otherwise  = rotL (No x left right) 
                           where d = desvio right

corrRight :: Arv a -> Arv a
corrRight (No x left right) | d == -1     = rotR (No x (rotL left) right)
                            | otherwise  = rotR (No x left right)
                            where d = desvio left

equi :: Arv a -> Arv a
equi t | d == -2     = corrLeft t  
       | d == 2      = corrRight t
       | otherwise   = t
       where d = desvio t


inserirAVL :: Ord a => a -> Arv a -> Arv a 
inserirAVL x Folha = No x Folha Folha
inserirAVL x (No a left right) | x < a         = equi (No a (inserirAVL x left) right)
                               | x > a         = equi (No a left (inserirAVL x right))
                               | otherwise     = No a left right

--77
removeAVL :: Ord a => a -> Arv a -> Arv a
removeAVL x (No a Folha Folha) = if x == a then Folha else No x Folha Folha
removeAVL x (No a left right) | x < a          = equi (No a (removeAVL x left) right)
                              | x > a          = equi (No a left (removeAVL x right))
                              | otherwise      = No y left (removeAVL y right) 
                              where y = maisEsqAVL right



---- Ficha ----

--70
sumArv :: Num a => Arv a -> a 
sumArv Folha = error "sumArv: Arvore vazia"
sumArv (No a Folha Folha) = a
sumArv (No a Folha right) = a + sumArv right
sumArv (No a left Folha) = a + sumArv left
sumArv (No a left right) = a + sumArv left + sumArv right

{- sumArv :: Num a => Arv a -> a
sumArv Folha = 0
sumArv (No x left right) = x + (sumArv left) + (sumArv right) -}

--71
listardecr :: Arv a -> [a]
listardecr Folha = []
listardecr (No a left right) = listardecr right ++ [a] ++ listardecr left

--72
nivel :: Int -> Arv a -> [a]
nivel _ Folha = []
nivel 0 (No x left right) = [x]
nivel n (No x left right) = nivel (n-1) left ++ nivel (n-1) right


--73
--a) mantem se depth log n, nivel AVL
--b) depth == number nodes
--c) depth log n como esperado

--74
mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Folha = Folha
mapArv f (No x left right) = No (f x) (mapArv f left) (mapArv f right)

--75
foldArv :: (a -> b -> b -> b) -> b -> Arv a -> b
foldArv f n Folha = n
foldArv f n (No x left right) = f x (foldArv f n left) (foldArv f n right)


{- Extra -}
--foldrArv
foldTreeR :: (a -> r -> r) -> r -> Arv a -> r
foldTreeR cons z t = foldArv g id t z           -- b ~ r -> r
                   where g a lt rt = lt . cons a . rt

--foldlArv
foldTreeL :: (acc -> a -> acc) -> acc -> Arv a -> acc
foldTreeL conj z t = foldArv g id t z           -- b ~ acc -> acc
                   where g a lt rt = rt . flip conj a . lt


simetrica :: Arv a -> Arv a
simetrica Folha = Folha
simetrica (No x left right) = (No x (simetrica right) (simetrica left))

{- TreeMap -}
data Tree k v = Leaf | Node (Tree k v) k v (Tree k v)

list :: Tree k v -> [(k,v)]
list Leaf = []
list (Node left a x right) = list left ++ [(a,x)] ++ list right
