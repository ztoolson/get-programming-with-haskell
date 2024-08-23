{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Semigroup
import Data.Text.Lazy as TL
import Data.Text.Lazy.IO as TLIO

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

-- quick check 23.1: create fourthWord once again, making the String type T.Text
fourthWord :: T.Text
fourthWord = T.pack thirdWord

-- note: need the overloaded strings language extension at the top of the file
aWord :: T.Text
aWord = "cheese"

-- main :: IO ()
-- main = do
--   print aWord

-- quick check 23.2: there's a language extension called `TemplateHaskell`. how would you compile template.hs to use this extension? how would you add it using a LANGUAGE pragma?
-- $ ghc template.hs -XTemplateHaskell
-- {-# TemplateHaskell #-}

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

someText :: T.Text
someText = "Some\ntext for\t you"

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

split = T.splitOn breakText exampleText

-- break up and combine Text using whitespace
newLines = T.unlines (T.lines sampleInput)
newSpaces = T.unwords (T.words someText)

-- Intercalate (opposite of splitOn)
combineSplit = T.intercalate breakText split

-- Monoid operations
-- ++ is for string type only
combined :: String
combined = "some" ++ " " ++ "strings"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

-- quick check 23.3: create your own version of T.lines and T.unlines by using splitOn and T.intercalate

myLines :: T.Text -> [T.Text]
myLines input = T.splitOn "\n" input

myUnlines :: [T.Text] -> T.Text
myUnlines input = T.intercalate "\n" input

-- Q23.1: see hello_world_text.hs
-- Q23.2: see lazy_sum.hs