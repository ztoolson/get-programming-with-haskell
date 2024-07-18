-- implement your own take
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- gcd
mygcd a b  = if remainder == 0
             then b
             else mygcd b remainder
    where remainder = a `mod` b


-- Q7.2 - Rewrite mygcd using pattern matching
mygcdpat a b | a `mod` b == 0 = b
mygcdpat a b = mygcdpat b (a `mod` b)


sayAmount n = case n of 
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

isEmpty [] = True
isEmpty _ = False

myHead [] = error "No head for empty list"
myHead (x:_) = x

-- Q7.1 - The tail function in Haskell returns an error when called on an empty list. Modify myTail so that it does handle the case of an empty list by returning the empty list
myTails [] = error "No tail for empty list"
myTail (_:xs) = xs