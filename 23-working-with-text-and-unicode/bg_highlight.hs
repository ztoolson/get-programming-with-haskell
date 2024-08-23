-- This program will take a text query and a body of text, and use curly braces,
-- {}, to highlight all cases of the word you're looking for.

-- for example, if "dog" is your query text, and your main text is "a dog walking dogs" you'd expect this output:
-- "a {dog} walking {dog}s"

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- highlight work in Sanskrit.
dharma :: T.Text
-- dharma = "धर्म"
dharma = "dog"

-- text to search. excerpt from Bhavagad Gita.

bgText :: T.Text
--bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"
bgText = "a dog walking dogs"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces = T.splitOn query fullText
        highlighted = mconcat ["{", query, "}"]

main = do
  TIO.putStrLn (highlight dharma bgText)