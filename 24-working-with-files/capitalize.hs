-- Q24.2 Write a program called capitalize.hs that will take a file as an argument, read that file, and then rewrite it capitalized.
--
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.IO qualified as TI
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFileName] -> do
      input <- TI.readFile inputFileName
      let capitalized = T.toUpper input
      TI.writeFile inputFileName capitalized
    _ -> putStrLn "Error: exactly one arguements required"
