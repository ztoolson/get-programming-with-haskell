-- Use Data.Text.Lazy and Data.Text.Lazy.IO to rewrite the lazy I/O secion from lession 22 by using the Text type
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Read as TLR

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main :: IO ()
main = do
    userInput <- TIO.getContents
    let numbers = toInts userInput
    print (sum numbers)