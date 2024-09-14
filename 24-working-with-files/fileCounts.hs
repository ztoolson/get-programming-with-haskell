-- use like: $ ./fileCounts hello.txt

import System.Environment (getArgs)
import System.IO

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = length input
    wordCount = (length . words) input
    lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =
  unwords
    [ "chars: ",
      show cc,
      "words: ",
      show wc,
      "lines: ",
      show lc
    ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary -- must print here to force summary to load
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
