module Lists where

-- Lesson 6: Lists

-- 6.1 The anatomy of a list

-- head is the first element in a list
value = head [1,2 ,3]
value' = head [[1,2],[3,4],[5,6]]


-- tail is the rest of the list
tail' = tail [1,2,3] -- [2,3]
anotherTail = tail [3] -- []

-- 6.2 Lists and Lazy evaluation
oneToTen = [1..10]

-- commong list functions
indexing = [1..10] !! 0 -- 1

len = length [2..10] -- 9
rev = reverse [1..10] -- [10,9,8,7,6,5,4,3,2,1]
isPalindrome word = word == reverse word

inList = elem 13 [0,13 .. 100] -- True
notInList = elem 'p' "cheese" -- False

respond phrase = if '!' `elem` phrase
    then "wow!"
    else "uh.. okay"

first5 = take 5 [2,4 .. 100] -- [2,4,6,8,10]
takeMore = take 10000000 [1] -- [1]

takeLast n aList = reverse (take n (reverse aList))

removeFirst5 = drop 5 [1..7] -- [6,7]

combineLists = zip [1,2,3] [2,4,6] -- [(1,2),(2,4),(3,6)]

takeFromInfiniteList n = take n (cycle [1])

-- imagine you want to divide a list of files and put them on n number of servers. or similarly split employees to n teams
assignToGroups n aList = zip groups aList
    where groups = cycle [1..n]

employeesAssignment = assignToGroups 2 ["John", "Zach", "Philip", "Jake", "Luis"]
serverAssignment = assignToGroups 3 ["file1.txt", "file2.txt", "file3.txt", "file4.txt", "file5.txt", "file6.txt", "file7.txt" ]


-- Q6.1: Haskell has a function called repeat that takes a value and repeats it infinitely. Using the functions you've learned so far, implement your own version of repeat
repeat' value = value : repeat' value


-- Q6.2: Write a function subseq that takes three arguments: a start position, an end posi- tion, and a list. The function should return the subsequence 
-- between the start and end. For example:
--  GHCi> subseq 2 5 [1 .. 10]
--  [3,4,5]
--  GHCi> subseq 2 7 "a puppy"
--  "puppy"

-- my solution
subsequence start end xs = drop start (reverse (drop (length xs - end) (reverse xs)))

-- my solution part 2
subseq start end xs = take (end - start) (drop start xs)


-- Q6.3 Write a function inFirstHalf that return True if an element is in the first half of the list, and otherwise returns false
inFirstHalf element xs = element `elem` take (length xs `div` 2) xs