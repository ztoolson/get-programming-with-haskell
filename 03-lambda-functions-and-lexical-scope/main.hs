main = do
    print $ sumSquareOrSquareSum 2 3
    print $ sumSquareOrSquareSum' 2 3

-- using a where statement
sumSquareOrSquareSum x y = if sumSquare > squareSum
                           then sumSquare
                           else squareSum
  where 
    sumSquare = x^2 + y^2
    squareSum = (x + y)^2

-- rewrite function without a "where" statement but using lambda's
sumSquareOrSquareSum' x y = (\sumSquare squareSum ->
    if sumSquare > squareSum
    then sumSquare
    else squareSum) (x^2 + y^2) ((x+y)^2)


-- write the function using "let" which is like a where but define the values first
sumSquareOrSquareSum'' x y = let sumSquare = (x^2 + y^2)
                                 squareSum = (x+y)^2
                             in if sumSquare > squareSum
                                then sumSquare
                                else squareSum


-- quick check 3.2
-- rewrite the following function to use a lambda function in place fo where

doubleDouble x = dubs * 2
    where dubs = x*2

doubleDouble' x = (\dubs -> dubs * 2) (x * 2)


-- overall this is a not useful function but can show how you can redfine variables in GHCi
overwrite x = let x = 2
    in
        let x = 3
            in
                let x = 4
                    in
                        x

-- rewrite overwrite but using only lambda's
overwrite' x = (\x -> 
        (\x ->
            (\x -> x) 4
        ) 3 
    ) 2


x = 4
-- uses 4 for the x
add1 y = y + x

-- ignores the x definition and uses x from the lambda
add2 y = (\x -> y + x) 3

-- ignores the arguement and uses the lambda y and x
add3 y = (\y -> (\x -> y + x) 1) 2


-- question 3.2. using a let expression and a lambda function aren't exactly the same thing under the hood. for example, the following code will run an error if you try to run it
counter x = let x = x + 1
    in
        let x = x + 1
            in
                x

counter' x = (\x -> 
                (\x -> x) x + 1
             ) x + 1