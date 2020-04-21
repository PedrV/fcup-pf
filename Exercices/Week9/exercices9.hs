{- ["Se " ++ show a ++ " elefantes incomodam muita gente,", show b ++ " elefantes incomodam muito mais !"] -}

--64 -90%

elefantes :: Int -> IO ()
elefantes n = do 
                let ys = map (\a -> [["Se " ++ (show a) ++ " elefantes incomadam muita gente, "],[(show (a+1)) ++ " incomodam muito mais !"]]) [1..n]
                sequence (map print (ys))
                return ()


--65 -99%

haskellwc :: IO ()
haskellwc = do
    content <- getContents
    let l = length $ lines content
    let w = words content
    let b = (countChars w) + (length w)
    putStrLn ((show l) ++ " lines" ++  " | " ++ (show (length w)) ++ " words" ++ " | " ++ (show b)  ++ " chars")
    return ()


countChars :: [String] -> Int
countChars [] = 0
countChars (xs:xss) = foldl (\count _ -> count+1 ) 0 xs + countChars xss


--66 -100%

invertLines :: IO () 
invertLines = do
    line <- getLine
    if (null line) then
        return ()
        else do
            putStrLn (invertLine line)
            invertLines

invertLine :: String -> String
invertLine s = unwords (map reverse (words s))

-- 2 forma

invertInput :: IO ()
invertInput = do
    content <- getContents
    let l = lines content
    putStrLn (invertLineInput l)
    return ()

invertLineInput :: [String] -> String
invertLineInput [] = []
invertLineInput (xs:xss) = unwords (map reverse (words xs)) ++ invertLineInput xss


--67 -100%

rot13 :: IO ()
rot13 = do
    line <- getLine
    if (null line) then
        return ()
        else do
            putStrLn (rot13String line)
            rot13

rot13String :: String -> String
rot13String [] = []
rot13String (s:ss) = if ( or [(elem s ['a'..'m']), (elem s ['A'..'M'])] ) then
    foldl (\a _ -> succ a) s [1..13] : rot13String ss
    else foldl (\a _ -> pred a) s [1..13] : rot13String ss


--68 - todas as peças funcionam falta junta-las

anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue (x:xs) | x == True      = True
               | otherwise      = anyTrue xs

adivinha :: IO ()
adivinha = do
    secretWord <- getLine
    putStr "\ESC[2J" -- ANSI to clear screen
    let start_word = foldl (\a _ -> '-' : a) [] [1.. (length secretWord)]
    start start_word secretWord 0
    return ()

-- dá "loop" uma vez a mais sempre que é dado um char (tentativa)
start :: String -> String -> Int -> IO ()
start start_word secretWord trys = do

    putChar '?'
    char <- getChar
    let appliedLetter = verify start_word secretWord char

    if appliedLetter == secretWord then
        do
            putStrLn (appliedLetter)
            putStrLn ("Adivinhou em " ++ (show trys) ++ "!")
        else 
            do
                if (appliedLetter == start_word) then
                    --putStrLn ("Nao ocorre")
                    -- putStrLn (appliedLetter)
                    start appliedLetter secretWord (trys+1)
                    else 
                        do
                            putStrLn (appliedLetter)
                            start appliedLetter secretWord (trys+1)
    return ()


verify :: String -> String -> Char -> String
verify cur ss x = let result = map ((==) x) ss in
        [if (fst y) then x else (!!) cur (snd y) | y<- zip result [0..]] -- ou usando o all mighty map com a ajuda de the best lambda functions: map (\a -> if a then x else '-') ss

-- 69 -100%

remove :: [Int] -> Int -> Int -> [Int]
remove (x:xs) n s | n == s    = (x-1) : xs
                  | otherwise = x : remove xs n (s+1)    
    
startNIM :: [Int] -> IO ()
startNIM xs = do 
    putStrLn ("5: " ++ (foldl (\a _ -> '*' : a) [] [1.. (last xs)]))
    putStrLn ("4: " ++ (foldl (\a _ -> '*' : a) [] [1.. ((!!) xs 3)]))
    putStrLn ("3: " ++ (foldl (\a _ -> '*' : a) [] [1.. ((!!) xs 2)]))
    putStrLn ("2: " ++ (foldl (\a _ -> '*' : a) [] [1.. ((!!) xs 1)]))
    putStrLn ("1: " ++ (foldl (\a _ -> '*' : a) [] [1.. (head xs)]))

    player1 <- readLn :: IO Int
    player2 <- readLn :: IO Int

    let x = remove xs (player1-1) 0
    let y = remove x (player2-1) 0

    if (all (\a -> a <= 0) x) then
        do
            putStrLn "Player 1 Wins!"
            return ()
            else
                if (all (\a -> a <= 0) y) then
                    do
                        putStrLn "Player 2 Wins!"
                        return ()
                        else
                            startNIM y

 
    return ()

nim :: IO ()
nim = do
    startNIM [1,2,3,4,5]
    return ()

    
main = do
    nim
