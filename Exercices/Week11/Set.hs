module Set (Set, empty, insert, member, remove, union, intersect, difference) where

data Set a = Bi a (Set a) (Set a) | EmptySet deriving (Eq,Ord)

instance Show a => Show (Set a) where
    show (Bi a l r) =  show $ listSet (Bi a l r)
    show EmptySet = []

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ qsort right
             where left = [ y | y<-xs, y < x]
                   right = [ y | y<-xs, y > x ] 

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

most_right :: Set a -> a
most_right (Bi x _ EmptySet) = x
most_right (Bi _ _ right) = most_right right

constructSet :: Ord a => [a] -> Set a
constructSet [] = EmptySet
constructSet xs = Bi a (constructSet left) (constructSet rigth)
                where a = (!!) list middle
                      middle = (length list) `div` 2
                      left = take middle list
                      r:rigth = drop middle list
                      list = qsort xs

remove :: Ord a => a -> Set a -> Set a
remove x EmptySet = EmptySet
remove x (Bi a EmptySet EmptySet) = if x == a then EmptySet else Bi a EmptySet EmptySet
remove x (Bi a left right) | x < a      = Bi a (remove x left) right
                           | x > a      = Bi a left (remove x right)
                           | otherwise  = Bi y (remove y left) right
                           where y = most_right left

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

-- ERROR on remove emptyset
difference :: Ord a => Set a -> Set a -> Set a
difference set EmptySet = set
difference EmptySet set = EmptySet
difference set1 set2 = foldl (\b a -> remove a b) set1 (listSet set)
                     where set = intersect set1 set2