import System.Environment
import Control.Monad

-- command line to print out argulemtns
-- ./sum 2 3 4 5
-- main :: IO ()
-- main = do
--   args <- getArgs
--   mapM_ putStrLn args

-- quick check 22.1: write a main that uses mapM to call getLine 3 times, and then use mapM_ to print out the values input.

exampleMain :: IO ()
exampleMain = do
  vals <- mapM (\_ -> getLine) [1..3]
  mapM_ putStrLn vals

-- use command line arg to say how many ints to sum
-- read in until we reach that number and print the sum
main :: IO()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)

-- quick chcek 22.2: write your own version of replicateM, myReplicateM, that uses mapM
myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n f = mapM (\_ -> f) [1..n]
