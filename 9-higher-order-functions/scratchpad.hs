import Data.Char (ord, chr)
 -- consider this. want to add 3 or mult 3 to all elements in a list. notice how the structure is very similar
add3ToAll [] = []
add3ToAll (x:xs) = (3 + x):add3ToAll xs

mult3ByAll [] = []
mult3ByAll (x:xs) = (3 * x):mult3ByAll xs


-- using map
reversed = map reverse ["dog", "cat", "moose"] -- ["god","tac","esoom"]
first = map head ["dog", "cat", "moose"] -- "dcm"
firstFour = map (take 4) ["pumpkin", "pie", "peanut butter"] -- ["pump","pie","pean"]

-- abstracting away recursion with map
addAnAMap = map ("a "++) ["train", "plane", "boat"]
squareAll = map (^2) [1,2,3]

-- non map addAnA
addAnA [] = []
addAnA (x:xs) = ("a " ++ x):addAnA xs

-- non map squareAll
squareAll' [] = []
squareAll' (x:xs) = x^2:squareAll' xs

-- implement map
myMap _ [] = []
myMap f (x:xs) = f x:myMap f xs

-- filtering a list
filteredEven = filter even [1,2,3,4]
filteredStartsWithA = filter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"]

-- implement filter
filter' _ [] = []
filter' f (x:xs) = if f x
    then x:filter' f xs
    else filter' f xs

-- quick chcek 9.1 - implement remove, which removes elements that pass the test
remove' _ [] = []
remove' f (x:xs) = if f x
    then remove' f xs
    else x:remove' f xs

-- folding a list. takes a list and reduces it to a single value
sum' = foldl (+) 0 [1,2,3,4]

-- the way foldl works is apply the binary argument to the intiial vlaue and the head of the list. that result is now the new initial value
-- quick check 9.2 - write a function myProduct which calulcates the product of a list of numbers
myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

-- it is common to use foldl and map together
-- sumOfSquares squares every value in the list and then takes the sum of it
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x
myReverse xs = foldl rcons [] xs

-- how to implement foldl
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x

-- Q9.1 - use filter and length to re-create the `elem` function
myElem char xs = length (filter (char==) xs) > 0
-- Q9.2 - your isPalindrome function doesn't handle sentences with spaces or capitals. use map and filter to make sure the phrase 
-- "A man with a plan a canal Panama" is recognized as a palindrome

isPalindrome a = a == reverse a

toLower c
    | c >= 'A' && c <= 'Z' = chr (ord c + 32)
    | otherwise  = c


updatedIsPalindrome a = cleaned == reverse cleaned
    where cleaned = filter (/= ' ')  (map toLower a)


-- Q9.3 - write a function harmonic that take sthe argument n and claculates the sum of the series to n. make sure to use lazy evaluation
-- the sum of 1/1 + 1/2 + 1/3 ...
harmonic n = foldr (+) 0 harmonicList
    where harmonicList = map (\x -> 1/x) [1..n]