--------------------------------------------------
----- This recap is for type and typeclasses -----  
--------------------------------------------------



-- type keyowrd is used to "rename" stuff, works like a synonym
type IntList = [Int]
type PhoneNumer = ([Int],String)

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

-- deriving typeclasses such as Eq, Show, 
data Persona = Persona {firstLastName :: String, pseudoName :: String, aage :: Int} deriving (Show,Eq)


makePersona :: String -> String -> Int -> Persona
makePersona flN pN age = Persona { firstLastName = flN, pseudoName = pN, aage = age }

getPseudo :: Persona -> String
getPseudo (Persona _ pN _) = pN


-- since Eq is derived, we can compare 2 Personas
-- (makePersona "Pedro Vieira" "Delicious" 18) == (makePersona "Pedro Vieira" "Delicious_2fast4you" 18) 


strings = [ x ++ [y] | x<-"":strings, y<-['a'..'z']]






-- references:
{- https://stackoverflow.com/questions/44965/what-is-a-monad?page=1&tab=votes#tab-top -}
{- https://stackoverflow.com/questions/36019513/first-time-using-the-maybe-type-in-haskell-is-my-understanding-wrong -}
{- https://stackoverflow.com/questions/18808258/what-does-the-just-syntax-mean-in-haskell -}
{- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types -}