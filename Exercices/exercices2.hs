--14
--a) [Char]
--b) (Char, Char, Char)
--c) [(True,Char)]
--d) ([Bool], [Char])
--e) [(a -> a)]
--f)

times2 :: Int -> Int
times2 x = 2*x

duasx :: (a -> a) -> a -> a
duasx f x = f (f x)
