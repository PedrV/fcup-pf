data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

-- Aux functions -- 

listTree :: Tree a -> [a]
listTree EmptyTree = []
listTree (Node a left right) = listTree left ++ [a] ++ listTree right 

-- assuming list is ordered --
constructTree :: [a] -> Tree a
constructTree [] = EmptyTree
constructTree xs = Node middle (constructTree left) (constructTree right)
                where middle = (!!) xs n
                      left = take n xs
                      (r:right) = drop n xs
                      n = ((length xs) `div` 2)




{- Link: https://videoconf-colibri.zoom.us/j/808939166
   Password: Curry1920 -}