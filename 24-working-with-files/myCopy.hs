-- Q24.1 Write a version of the unix program that will copy a file and allow you to rename it (just mimic the basic functionality and don't worry about specific flags)
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
    [inputFileName, copyFileName] -> do
      input <- TI.readFile inputFileName
      TI.appendFile copyFileName input
    _ -> putStrLn "Error: exactly two arguements required"
