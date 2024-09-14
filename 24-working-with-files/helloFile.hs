import System.IO

-- opening and closing files

main :: IO ()
main = do
  myFile <- openFile "hello.txt" ReadMode

  hasLine <- hIsEOF myFile

  firstLine <-
    if not hasLine
      then hGetLine myFile
      else return "empty"

  putStrLn firstLine

  hasSecondLine <- hIsEOF myFile

  secondLine <-
    if not hasSecondLine
      then hGetLine myFile
      else return "empty"

  goodByeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodByeFile secondLine

  hClose myFile
  hClose goodByeFile

  putStrLn "done!"
