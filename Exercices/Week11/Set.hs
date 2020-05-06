module Set (Set, empty, insert, member, union, intersect) where

data Set a = Bi a (Set a) (Set a) | EmptySet deriving (Eq,Ord)

instance Show a => Show (Set a) where
    show (Bi a l r) = show $ listSet (Bi a l r)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique [y | y<-xs, y /= x]

listSet :: Set a -> [a]
listSet EmptySet = []
listSet (Bi a left rigth) = listSet left ++ [a] ++ listSet rigth

isEmpty :: Set a -> Bool
isEmpty EmptySet = True
isEmpty _ = False

empty :: Set a
empty = EmptySet

constructSet :: Eq a => [a] -> Set a
constructSet [] = EmptySet
constructSet xs = Bi a (constructSet left) (constructSet rigth)
                where a = (!!) list middle
                      middle = (length list) `div` 2
                      left = take middle list
                      r:rigth = drop middle list
                      list = unique xs


insert :: Ord a => a -> Set a -> Set a
insert x EmptySet = Bi x EmptySet EmptySet
insert x (Bi a left rigth) | x < a      = Bi a (insert x left) rigth
                           | x > a      = Bi a left (insert x rigth)
                           | otherwise  = Bi a left rigth

member :: Ord a => a -> Set a -> Bool
member x EmptySet = False
member x (Bi a left rigth) | x < a     = (member x left)
                           | x > a     = (member x rigth)
                           | otherwise = True


union :: Ord a => Set a -> Set a -> Set a
union set EmptySet = set
union EmptySet set = set
union (Bi x l1 r1) (Bi y l2 r2) = union l1 $ union r1 (insert x (Bi y l2 r2))

intersect' :: Ord a => Set a -> Set a -> Set a -> Set a
intersect' set EmptySet cur = cur
intersect' EmptySet set cur = cur
intersect' (Bi x l1 r1) set cur | member x set       = intersect' r1 set (intersect' l1 set (insert x cur))
                                | otherwise          = intersect' r1 set (intersect' l1 set cur)

intersect :: Ord a => Set a -> Set a -> Set a
intersect s1 s2 =  intersect' s1 s2 empty
