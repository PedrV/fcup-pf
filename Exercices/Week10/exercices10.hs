data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

-- 1ª parte Week10 - Implementar basico arvores binarias e AVL --

listTree :: Tree a -> [a]
listTree EmptyTree = []
listTree (Node a left right) = listTree left ++ [a] ++ listTree right 



-- Quicksort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ll ++ [x] ++ qsort rl
            where ll = [y | y<-xs, y < x]
                  rl = [y | y<-xs, y >= x]

-- Recursive removing duplicates for quicksort
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates ([y | y<-xs, y /= x])



constructTree :: Ord a => [a] -> Tree a
constructTree [] = EmptyTree
constructTree (xs) = Node middle (constructTree left_side) (constructTree right_side)
                        where middle = (!!) list l
                              left_side = take l list
                              r:right_side = drop l list
                              l = ((length list) `div` 2)
                              list = qsort $ removeDuplicates xs


findElementTree :: Ord a => Tree a -> a -> Bool
findElementTree EmptyTree _ = False
findElementTree (Node a (leftTree) (rightTree)) x | x == a        = True 
                                                  | x < a         = findElementTree leftTree x
                                                  | x > a         = findElementTree rightTree x

-- 76
smallestElement :: Ord a => Tree a -> a
smallestElement EmptyTree = error "smallestElement: EmptyTree does not have elements"
smallestElement (Node a (leftTree) (rightTree)) | leftTree == EmptyTree       = a
                                                | otherwise                   = smallestElement leftTree

-- 76
biggestElement :: Ord a => Tree a -> a
biggestElement EmptyTree = error "biggestElement: EmptyTree does not have elements"
biggestElement (Node a (leftTree) (rightTree)) | rightTree == EmptyTree       = a
                                               | otherwise                    = biggestElement rightTree


insertElement :: Ord a => Tree a -> a -> Tree a
insertElement EmptyTree x = (Node x EmptyTree EmptyTree)
insertElement (Node a (leftTree) (rightTree)) x | x < a       = (Node a (insertElement leftTree x) (rightTree))
                                                | x > a       = (Node a (leftTree) (insertElement rightTree x))
                                                | x == a      = (Node a leftTree rightTree)


removeElement :: Ord a => Tree a -> a -> Tree a
removeElement EmptyTree _ = EmptyTree
removeElement (Node a EmptyTree EmptyTree) x = if a == x then EmptyTree else Node a EmptyTree EmptyTree -- Not just EmptyTree
removeElement (Node a (leftTree) (rightTree)) x | x == a          = Node y (removeElement leftTree y) rightTree
                                                | x < a           = Node a (removeElement leftTree x) rightTree
                                                | x > a           = Node a leftTree (removeElement rightTree x)
                                                where y = biggestElement leftTree

treeHeigth :: Ord a => Tree a -> Int
treeHeigth EmptyTree = 0
treeHeigth (Node a (leftTree) (rightTree)) = max (1 + treeHeigth leftTree) (1 + treeHeigth rightTree)

isBalanced :: Ord a => Tree a -> Bool
isBalanced EmptyTree = True
isBalanced (Node a (leftTree) (rightTree)) = abs ( (treeHeigth leftTree) - (treeHeigth rightTree) ) <= 1 && (isBalanced leftTree) && (isBalanced rightTree) 



----- Arvores AVL -----

-- Simetrico há constructTree
constructTreeAVL :: Ord a => [a] -> Tree a
constructTreeAVL [] = EmptyTree
constructTreeAVL xs = Node middle (constructTreeAVL left) (constructTreeAVL right)
                  where middle = (!!) list l
                        left = take l list
                        r:right = drop l list
                        l = length list `div` 2
                        list = qsort $ removeDuplicates xs

-- Simetrico há findElementTree
searchAVL :: Ord a => Tree a -> a -> Bool
searchAVL EmptyTree _ = False
searchAVL (Node a (leftTree) (rightTree)) x | x == a        = True
                                            | x > a         = searchAVL rightTree x
                                            | x < a         = searchAVL leftTree x

-- Simetrico treeHeight
heigthAVL :: Tree a -> Int 
heigthAVL EmptyTree = 0
heigthAVL (Node a (leftTree) (rightTree)) = max (1 + heigthAVL leftTree) (1 + heigthAVL rightTree) 


{- desvio :: Tree a -> Int
desvio (Node _ (left_side) (right_side)) = heigthAVL left_side - heigthAVL right_side


rotRight :: Tree a -> Tree a
rotRight (Node x (Node y t1 t2) t3) = Node y t1 (Node x t2 t3) 
rotRight t = t


rotLeft :: Tree a -> Tree a
rotLeft (Node x t1 (Node y t2 t3)) = Node y (Node x t1 t2) t3
rotLeft t = t 



correctDeviationRigth :: Tree a -> Tree a
correctDeviationRigth (Node a (leftTree) (rightTree)) | desvio leftTree == -1       = rotRight (Node a (rotLeft leftTree) rightTree)
                                                      | otherwise                   = rotRight (Node a leftTree rightTree)
correctDeviationRigth tree = tree 


correctDeviationLeft :: Tree a -> Tree a
correctDeviationLeft (Node a (leftTree) (rightTree)) | desvio rightTree == 1        = rotLeft (Node a leftTree (rotRight rightTree))
                                                     | otherwise                    = rotLeft (Node a leftTree rightTree)
correctDeviationLeft tree = tree



reEqui :: Tree a -> Tree a
reEqui tree | des == 2     = correctDeviationRigth tree
            | des == -2    = correctDeviationLeft tree 
            | otherwise    = tree
            where des = desvio tree


insertElemAVL :: Ord a => Tree a -> a -> Tree a
insertElemAVL EmptyTree x = Node x EmptyTree EmptyTree
insertElemAVL (Node a (leftTree) (rightTree)) x | x < a           = reEqui $ Node a (insertElemAVL leftTree x) rightTree
                                                | x > a           = reEqui $ Node a leftTree (insertElemAVL rightTree x)
                                                | x == a          = Node a leftTree rightTree 

-- 77
removeElemAVL :: Ord a => Tree a -> a -> Tree a
removeElemAVL EmptyTree _ = EmptyTree
removeElemAVL (Node a EmptyTree EmptyTree) x = if a == x then EmptyTree else Node a EmptyTree EmptyTree -- Not just EmptyTree
removeElemAVL (Node a (leftTree) (rightTree)) x | x == a          = Node y leftTree (removeElemAVL rightTree y)
                                                | x < a           = reEqui $ Node a (removeElemAVL leftTree x) rightTree
                                                | x > a           = reEqui $ Node a leftTree (removeElemAVL rightTree x)
                                                where y = smallestElement rightTree
-}                                                 


deviation :: Tree a -> Int
deviation (Node _ leftTree rightTree) = heigthAVL leftTree - heigthAVL rightTree


rotR :: Tree a -> Tree a
rotR (Node x (Node y t1 t2) t3) = Node y t1 (Node x t2 t3) 
rotR tree = tree

rotL :: Tree a -> Tree a
rotL (Node x t1 (Node y t2 t3)) = Node y (Node x t1 t2) t3
rotL tree = tree

correctRightDev :: Tree a -> Tree a
correctRightDev (Node a leftTree rightTree) | d == -1          = rotR (Node a (rotL leftTree) rightTree)
                                            | otherwise        = rotR (Node a leftTree rightTree)
                                            where d = deviation leftTree
correctRightDev tree = tree 


correctLeftDev :: Tree a -> Tree a
correctLeftDev (Node a leftTree rightTree) | d == 1            = rotL (Node a leftTree (rotR rightTree)) 
                                           | otherwise         = rotL (Node a leftTree rightTree)
                                           where d =  deviation rightTree
correctLeftDev tree = tree


equi :: Tree a -> Tree a
equi tree | d == 2      = correctRightDev tree
          | d == -2     = correctLeftDev tree
          | otherwise   = tree
          where d = deviation tree


insertEle :: Ord a => Tree a -> a -> Tree a
insertEle EmptyTree x = Node x EmptyTree EmptyTree
insertEle (Node a leftTree rightTree) x | x < a       = equi $ Node a (insertEle leftTree x) rightTree   
                                        | x > a       = equi $ Node a leftTree (insertEle rightTree x)
                                        | x == a      = Node a leftTree rightTree

-- 77
removeEle :: Ord a => Tree a -> a -> Tree a
removeEle EmptyTree _ = EmptyTree
removeEle (Node a EmptyTree EmptyTree) x = if a == x then EmptyTree else Node a EmptyTree EmptyTree
removeEle (Node a leftTree rightTree) x | x == a      = Node y (removeEle leftTree y) rightTree
                                        | x > a       = equi $ Node a leftTree (removeEle rightTree x)
                                        | x < a       = equi $ Node a (removeEle leftTree x) rightTree
                                        where y = biggestElement leftTree


-- 2ª Parte Week10 - Exericios folha --

--70

sumArv :: Num a => Tree a -> a
sumArv EmptyTree = 0
sumArv (Node a leftTree rightTree) = a + sumArv leftTree + sumArv rightTree


--71
listDecres :: Tree a -> [a]
listDecres EmptyTree = []
listDecres (Node a leftTree rightTree) = listDecres rightTree ++ [a] ++ listDecres leftTree

--72
nivel :: Int -> Tree a -> [a]
nivel n EmptyTree = []
nivel n (Node a leftTree rightTree) | n == 0     = listTree (Node a EmptyTree EmptyTree)
                                    | otherwise  = (nivel (n-1) leftTree) ++ (nivel (n-1) rightTree)

--73
--a)
insertPartition :: Ord a => [a] -> Tree a
insertPartition xs = constructTree xs

--b)
simpleInsert :: Ord a => [a] -> Tree a
simpleInsert xs = foldr (\a b -> insertElement b a) EmptyTree xs

--c)
insertAVL :: Ord a => [a] -> Tree a
insertAVL xs = foldr (\a b -> insertEle b a) EmptyTree xs


--74
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node a EmptyTree EmptyTree) = Node (f a) EmptyTree EmptyTree   
mapTree f (Node a EmptyTree rightTree) = Node (f a) EmptyTree (mapTree f rightTree)
mapTree f (Node a leftTree EmptyTree) = Node (f a) (mapTree f leftTree) EmptyTree
mapTree f (Node a leftTree rightTree) = Node (f a) (mapTree f leftTree) (mapTree f rightTree)

--75 (partial correct)
foldrTree f p (Node a EmptyTree EmptyTree) = f p a
foldrTree f p (Node a EmptyTree rightTree) = f a (foldrTree f p rightTree)
foldrTree f p (Node a leftTree EmptyTree) = f a (foldrTree f p leftTree)
foldrTree f p (Node a leftTree rightTree) = f a (foldrTree f (foldrTree f p leftTree) rightTree)


foldr' f p [] = p
foldr' f p (x:xs) = f x (foldr' f p xs)    