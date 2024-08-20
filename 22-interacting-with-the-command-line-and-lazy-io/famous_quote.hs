main :: IO ()
main = do
  putStr "Select a number between 1 and 5: "
  selection <- getLine
  let quote = getQuote (read selection :: Int)
  putStrLn quote
  putStr "Would you like another? (y/n): "
  response <- getLine
  if response == "n"
    then return ()
    else main

getQuote :: Int -> String
getQuote 1 = "The only true wisdom is in knowing you know nothing."
getQuote 2 = "The unexamined life is not worth living."
getQuote 3 = "Know thyself."
getQuote 4 = "The greatest wealth is to live content with little."
getQuote 5 = "The most valuable possession you can own is an open heart."