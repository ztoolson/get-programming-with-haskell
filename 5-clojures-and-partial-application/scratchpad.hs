-- defined in chapter 4
ifEven myFunction x = if even x
    then myFunction x
    else x
inc n = n + 1

genIfEven f = (\x -> ifEven f x)

-- ifEvenInc = (\x -> ifEven inc x)
-- ifEvenInc = genIfEven inc

-- quick check 5.1: Write a function genIfXEven that creates a closure with x and returns a new function that allows the user to pass in a function to apply to x if x is even.
genIfXEven x = (\f -> ifEven f x)

-- 5.2 getRequestURL
getRequestURL host apiKey resource id = host ++
    "/" ++
    resource ++
    "/" ++
    id ++ 
    "?token=" ++
    apiKey

-- note: in haskell, order of arguements isn't the same order that they are used. anytime you want to use a closure (which in haskell is pretty much anytime)
-- you want to order your arguments from most to least general
-- in this example, host is the most general because it can have mutiple api keys, each api key is going to have access to different resources, and each resource is going to have a specific id associated with it

genHostRequestBuilder host = (\apiKey resource id ->
    getRequestURL host apiKey resource id)

genAPIRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey id)

exampleURLBuilder = genHostRequestBuilder "http://example.com"

myExampleURLBuilder = genAPIRequestBuilder exampleURLBuilder "1337hAsk3ll"

-- write a version of genAPIRequestBuilder that also takes the resuource as the arguement
getResourceRequestBuilder hostBuilder apiKey resource =  (\id -> hostBuilder apiKey resource id)

add4 a b c d = a + b + c + d
addXto3 x = (\b c d -> add4 x b c d)

addXYto2 x y = (\c d -> add4 x y c d)


mystery = add4 3

flipBinaryArgs binaryFunction = \x y -> binaryFunction y x

-- Q5.1: Now that you know about partial application, you no longer need to use genIfEvenX. Redefine ifEvenInc, ifEvenDouble, and ifEvenSquare by using ifEven and partial application.
ifEvenInc = ifEven (\n -> n + 1)
ifEvenDouble = ifEven (\n -> 2*n)
ifEvenSquare = ifEven (\n -> n*n)

-- Q5.2: Even if Haskell didn’t have partial application, you could hack together some approximations.
-- Following a similar pattern to flipBinaryArgs (figure 5.6), write a func- tion binaryPartialApplication that takes a binary function 
-- and one argument and returns a new function waiting for the missing argument.

binaryPartialApplication f arg1 = (\arg2 -> f arg1 arg2)