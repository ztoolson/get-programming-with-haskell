-- myAverage aList = sum aList / length aList
myAverage aList = sum aList / fromIntegral (length aList)


-- Int type is based on computer architecture and has bounds based on memeory limitations
x :: Int -- x here is the variable name :: Int is the variable type. the entire line is the type signature
x = 2 -- x here is the variable definition

outOfBounds = x^2000 -- will truncate to 0

-- Integer type for closely resembles the typical Haskell way for thinking about types
y :: Integer
y = 2

largeNumber = y^2000

-- common types. Char, Double, and Bool
letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

-- List types
values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8, 0.85]

letters :: [Char]
letters = ['a','b','c']

-- is [Char] the same as String? yes
isString = letters == "abc"

aPet :: [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

-- Tuple types
ageAndHeight :: (Int,Int)
ageAndHeight = (35, 76)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch", 'D')

streetAddress :: (Int, String)
streetAddress = (123, "Happy St.")

-- Functions also have type signatures
double :: Int -> Int -- arguement of type Int that returns an Int
double n = n*2

-- Converting from one type to another with half
half :: Int -> Double
-- half n = n / 2 -- this is incorrect
half n = fromIntegral n / 2 -- here you have transformed n from an Int t o a more general number

-- quick check 11.1. write `halve`, which uses div instead and include a type signature
halve :: Integer -> Integer
halve n = n `div` 2

-- functions for converting to and from strings

-- quick check 11.2
printDouble :: Int -> String
printDouble n = show (n*2)

-- with no type signature, if the variable is used in other places Haskell has enough information to figure it out
z = read "6"
q = z / 2

anotherNumber :: Int
anotherNumber = read "6"

-- functions with multiple arguements
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

-- why does haskell do this? all functions with multiple arguements are rewritten like this
makeAddressLambda :: Int -> String -> String -> (Int, String, String)
makeAddressLambda =
    (\number ->
        (\street ->
            \town -> (number, street, town)
        )
    )

-- example of how to call it
lambdaAddress = (((makeAddressLambda 123) "Happy St.") "Haskell Town")
address = (((makeAddress 123) "Happy St.") "Haskell Town") -- notice it is the same as the lambda? neat


-- types for first-class functions
ifEven :: (Int -> Int) -> Int -> Int
ifEven f n =
    if even n
    then f n
    else n

-- type variables
simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

simple :: a -> a -- type variables are literally variables for types. instead of representing a value, they represent a type
simple x = x

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)


-- Q11.1 What is the type signature for filter?
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f (x:xs) =
    if f x
    then x:myFilter f xs
    else myFilter f xs

-- Q11.2 - In haskell, both tail and head have na error when called on the empty list.
-- You can write a version of tail that won't fail but instead return an empty list
tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

-- Can you write a version of head that returns an empty list when called on an empty list?
head' :: [a] -> [a]
head' [] = []
head' (x:xs) = [x]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- Q11.3 - Recall myFoldl from lesson 9
myFoldl :: (b -> a -> b) ->  b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x