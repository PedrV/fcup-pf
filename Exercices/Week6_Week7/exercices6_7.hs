--47

listcomp :: (a -> a) -> (a -> Bool) -> [a] -> [a]
listcomp f p xs = filter p $ map f xs