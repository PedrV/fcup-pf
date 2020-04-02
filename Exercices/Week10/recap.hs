--------------------------------------------------
----- This recap is for type and typeclasses -----  
--------------------------------------------------


-- type keyowrd is used to "rename" stuff, works like a synonym
type IntList = [Int]
type PhoneNumer = ([Int],String)
type PairList k v = [(k,v)]

mult2 :: IntList -> IntList
mult2 xs = map (*2) xs

concat2 :: IntList -> IntList -> IntList
concat2 xs ys = xs ++ ys


-- Algebric Data Types --

{- Just like Bool is something like: data Bool = True | False -}
{-                                   data Int  = minInt | minInt-1 | ... | 0 | ... | maxInt-1 | maxInt -}
{- Something like that  -}


data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show

surfaces :: Shape -> Float
surfaces (Circle _ r) = 3.14 * r * r
surfaces (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1-x2) * (abs y1-y2)


-- deslocate shape

moveShape :: Shape -> Float -> Float -> Shape
moveShape (Circle (Point x1 y1) r) mx my = Circle (Point (x1+mx) (y1+my)) r
moveShape (Rectangle (Point x1 y1) (Point x2 y2)) mx my = Rectangle (Point (x1+mx) (y1+my)) (Point (x2+mx) (y2+my)) 

-- person

data Person = Person String String Int Float Int String deriving Show

makePerson :: String -> String -> Int -> Float -> Int -> String -> Person
makePerson firstname lastname age height phonenumber flavor = Person firstname lastname age height phonenumber flavor

firstName :: Person -> String
firstName (Person firstname _ _ _ _ _ ) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _ ) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _ )  = age

height :: Person -> Float
height (Person _ _ _ height _ _ ) = height

phoneNumber :: Person -> Int
phoneNumber (Person _ _ _ _ phonenumber _ ) = phonenumber

favoriteFlavor :: Person -> String
favoriteFlavor (Person _ _ _ _ _ flavor ) = flavor


-- car

-- record syntax
data Car = Car {company :: String, model :: String, year :: Int} deriving Show

-- does not have to be in order as long as all of them are there, otherwise it will compile and run but an error "Missing filed" is thrown
makeCar :: String -> String -> Int -> Car
makeCar brand model year = Car {company = brand, year = year, model = model} 

getCarYear :: Car -> Int
getCarYear (Car _ _ year) = year 


-- testing maybe

maybeSqrt :: Float -> Maybe Float
maybeSqrt x
        | x >= 0 = Just (sqrt x)
        | otherwise = Nothing

maybeInt :: Maybe Int  -> Maybe Int
maybeInt n = case n of
            Just n
                | n >= 0      -> Just n
                | otherwise   -> error "Not positive"
            Nothing -> error "Not int passed"


-- type constructors--      -- value constructrurs --
{- data Something a    =    Else a a a | Not a a a | ... deriving Show -}

{- Each value constructur can have multiple values or be an empty value constructor -}

-- paremeterized types constructurs
data Vector a = Vector a a a deriving Show

data RecordVector a = RecordVector {x :: a, y :: a, z :: a} deriving Show

vecSum :: (Num t) => Vector t -> Vector t -> Vector t
vecSum (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

vecMult :: (Num t) => RecordVector t -> RecordVector t -> RecordVector t
vecMult (RecordVector x1 y1 z1) (RecordVector x2 y2 z2) = RecordVector {x = (x1*x2), y = (y1*y2), z = (z1*z2)}

vecSka :: (Num t) => Vector t -> t -> Vector t 
vecSka (Vector x y z) k = Vector (x*k) (y*k) (z*k)


-- Typeclasses in ADT -- 

{- deriving typeclasses such as Eq, Show, haskell will check if all the parameters are of the typeclass Show and Eq, in this case
   String and Int are both from Eq an Show-}
data Persona = Persona {firstLastName :: String, pseudoName :: String, aage :: Int} deriving (Show,Eq)


makePersona :: String -> String -> Int -> Persona
makePersona flN pN age = Persona { firstLastName = flN, pseudoName = pN, aage = age }

getPseudo :: Persona -> String
getPseudo (Persona _ pN _) = pN


-- since Eq is derived, we can compare 2 Personas
-- (makePersona "Pedro Vieira" "Delicious" 18) == (makePersona "Pedro Vieira" "Delicious_2fast4you" 18) 


-- Enum will check if every value constructur has empty parameters
data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq, Ord, Enum, Bounded)


data Eithere a b = Lefte a | Righte b deriving (Show, Eq, Ord)


intPos :: Int -> Eithere Int String
intPos n | n >= 0       = Lefte n 
         | otherwise    = Righte "Right pls"  

name :: Eithere Int String -> String -- patter matching
name (Lefte n)  = "Pedro"
name (Righte x) = "Vieira"

name' :: Eithere Int String -> String -- get value and case
name' n = case n of 
            Lefte n -> "Int"
            Righte n -> "String"

ints :: Eithere Int String -> Eithere Int String
ints n = case n of
            Lefte n
                | n >= 0      -> Lefte n
                | otherwise   -> Righte "Not positive"
            Righte n -> Righte "Not int passed"


-- Recursive type constructor -- 

{- infix specifies the precedence of an operator
   - infixl _ _ the l says the operator (3rd field) is left associative
   - infixr _ _ the r says the operator is right associative  
   - infix_ 0-9 _ the number tells the precedence, "priority", to other operators 
   - infix_ _ ? specifies the operator symbol
   if infix is passed, the associativty is non
 If the digit is omitted, level 9 is assumed. Any operator lacking a fixity declaration is assumed to be infixl 9 -} 



infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Eq, Ord)  

lists :: Int -> List Int
lists n = n :-: n :-: n :-: (42 :-: Empty)



-- Trees --


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

checkOrd :: Ord a => [a] -> Bool
checkOrd [] = True
checkOrd [x] = True
checkOrd (x:y:xs) | x <= y      = checkOrd (y:xs)
                  | otherwise   = False  

treeList :: Tree a -> [a]
treeList EmptyTree = []
treeList (Node x left right) = treeList left ++ [x] ++ treeList right -- put root on top and list the left tree and right tree

isTreeOrd :: Ord a => Tree a -> Bool
isTreeOrd EmptyTree = True
isTreeOrd (Node x left right) = checkOrd $ treeList (Node x left right)

isTreeOrd' :: Ord a => Tree a -> Bool
isTreeOrd' EmptyTree = True
isTreeOrd' tree = checkOrd $ treeList tree -- não vamos usar "patter matching" da tree para nada por isso podemos só escrever
                                           -- tree (ou outra coisa qualquer) que ele já sabe, pelo tipo, aquilo que esperar


findElementTree :: Ord a => Tree a -> a -> Bool
findElementTree EmptyTree n = False
findElementTree (Node x left right) n | n == x      = True
                                      | n < x       = findElementTree left n  
                                      | n > x       = findElementTree right n


{- Only for style -}
-- encapsulamento da função com parametro extra para o nivel
theFindElementTree' :: Ord a => Tree a -> a -> Int -> (Bool,Int)
theFindElementTree' EmptyTree n a = (False,(-1))
theFindElementTree' (Node x left right) n a | n == x      = (True,a)
                                            | n < x       = theFindElementTree' left n (a+1)  
                                            | n > x       = theFindElementTree' right n (a+1)


findElementTree' :: Ord a => Tree a -> a -> (Bool,Int) 
findElementTree' tree n = theFindElementTree' tree n 0
{- Only for style -}


insertElementTree :: Ord a => Tree a -> a -> Tree a
insertElementTree EmptyTree n = Node n EmptyTree EmptyTree
insertElementTree (Node x left right) n | n == x     = Node x left right
                                        | n < x      = Node x (insertElementTree left n) right
                                        | n > x      = Node x left (insertElementTree right n) 


constructTree :: Ord a => [a] -> Tree a
constructTree [] = EmptyTree
constructTree xs = Node middle (constructTree left) (constructTree right)
                       where middle = (!!) xs n
                             left = take n xs
                             (r:right) = drop n xs -- drop takes the element in the middle with him, so the construct would not stop, so r:right is to drop
                             n = ((length xs) `div` 2)

smallestEle :: Ord a => Tree a -> a
smallestEle (Node x EmptyTree EmptyTree) = x
smallestEle (Node x left right) = smallestEle left

biggestEle :: Ord a => Tree a -> a
biggestEle (Node x EmptyTree EmptyTree) = x
biggestEle (Node x left right) = biggestEle right


removeElementTree :: Ord a => Tree a -> a -> Tree a
removeElementTree EmptyTree _ = EmptyTree
removeElementTree (Node x EmptyTree EmptyTree) n | n == x       = EmptyTree
                                                 | otherwise    = Node x EmptyTree EmptyTree

removeElementTree (Node x left right) n | n == x      = Node (smallestEle right) left (removeElementTree right (smallestEle right)) 
                                        | n < x       = Node x (removeElementTree left n) right
                                        | n > x       = Node x left (removeElementTree right n)


treeHeigth :: Tree a -> Int
treeHeigth EmptyTree = 0
treeHeigth (Node x left right) = (+1) (max (treeHeigth left) (treeHeigth right))


isBalanced :: Tree a -> Bool
isBalanced EmptyTree = True
isBalanced (Node x left right) | d <= 1 && (isBalanced left) && (isBalanced right)     = True 
                               | otherwise                                             = False
                               where d = abs ((treeHeigth left)-(treeHeigth right))     




{- the infamous tree built by hand:
(Node 20 (Node 3 (Node 2 (Node 1 EmptyTree EmptyTree) EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 42 (Node 21 EmptyTree EmptyTree) (Node 777 EmptyTree EmptyTree))) -}



strings = [ x ++ [y] | x<-"":strings, y<-['a'..'z']]






-- references:
{- https://stackoverflow.com/questions/44965/what-is-a-monad?page=1&tab=votes#tab-top -}
{- https://stackoverflow.com/questions/36019513/first-time-using-the-maybe-type-in-haskell-is-my-understanding-wrong -}
{- https://stackoverflow.com/questions/18808258/what-does-the-just-syntax-mean-in-haskell -}
{- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types -}