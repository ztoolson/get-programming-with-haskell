import System.Random 
import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = mconcat ["hello ", name, " !"]

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

-- quick check 21.1 - which line retrieves the user input? what do you assume that input is?
-- `name <- getLine`. assume name is a String (but it actually is an IO String)

-- quick check 21.3 - could you simplify your code to combine helloPerson and getLine like this?
-- let statement = helloPerson getLine
-- A: no because getLine type is :: IO String but moving the name <- getLine allows you to treat name like a string even though the type is IO String0


minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

rollDie :: IO ()
rollDie = do
    dieRoll <- randomRIO (minDie, maxDie)
    putStrLn (show dieRoll)

-- quick check 21.2 - is it ok if the last line in your main is getLine?
-- no, because the type of main is IO () but the type of getLine is IO string
-- The last statement in a 'do' block must be an expression

-- Q21.1: Translate helloPerson and main into code by using do-notation in a Maybe. Assume that all the 
-- user input is replaced with a Map with a value for the input. Ignore the first putStrLn and simply return the statement at the end.

nameMap :: Map.Map String String
nameMap = Map.fromList [("test", "maybe name")]

maybeMain :: Maybe String
maybeMain = do
    name <- Map.lookup "test" nameMap
    let statement = helloPerson name
    return statement

-- Q21.2 Create a program that asks the user to input a number and then returns the nth Fibonacci numbers (see lesson 8 for an example of computing Fibonacci numbers).
fastFib n1 _ 1 = n1
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)

userFib :: IO ()
userFib = do
    putStrLn "Enter a number "
    numInput <- getLine
    let num = read numInput :: Int
    let fibResult = fastFib 1 1 num
    putStrLn (show fibResult)

