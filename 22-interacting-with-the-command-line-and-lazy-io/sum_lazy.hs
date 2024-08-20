import Control.Monad

-- $ ./sum_lazy
--  4
--  234
--  23
--  1
--  3
--  <ctrl-d>
-- main :: IO ()
-- main = do
--   userInput <- getContents
--   let numbers = toInts userInput
--   print (sum numbers)

-- quick check 22.3: use lazy I/O to write a program that reversed your input and prints it back to you
-- main :: IO ()
-- main = do
--   forever $ do
--     userInput <- getLine
--     putStrLn (reverse userInput)

toInts :: String -> [Int]
toInts = map read . lines

-- quick check 22.4: write a program that returns the sum of squares of the input
main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum (map (^2) numbers))

