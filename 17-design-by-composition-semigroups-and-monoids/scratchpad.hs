import Prelude
import Data.Semigroup
import Data.List (sort)
-- consider this, so far when you've combined multiple strings, you've used `++`. this can be tedious for larger strings:
-- "this" ++ " " ++ "is" ++ " " ++ "a" ++ " " ++ "bit" ++ " " ++ "much"
-- is there a better way to solve this?

-- combining functions with `.`
myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

-- myAll tests that a property is true of all items in a list
myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

-- Quick check 17.1: Impelemnt `myAny` by using function composition. `myAny` tests that a property is True for at least one value in the list
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)


-- you can use the "instance" keyword to make Integer an instance of the Semigroup type class.
instance Semigroup Integer where
    -- general type (<>) :: Semigroup a => a -> a -> a
    (<>) :: Integer -> Integer -> Integer
    (<>) x y = x + y -- you define the <> operator as simple addition

-- Quick check 17.2: can you use (/) to make Int into a Semigropu
-- No, because devision doesn't always return an Int type

-- the color semigroup
data Color = Red |
    Yellow |
    Blue   |
    Green  |
    Purple |
    Orange |
    Brown  |
    Black deriving (Show, Eq)

-- instance Semigroup Color where
--     (<>) :: Color -> Color -> Color
--     (<>) Red Blue = Purple
--     (<>) Blue Red = Purple
--     (<>) Yellow Blue = Green
--     (<>) Blue Yellow = Green
--     (<>) Yellow Red = Orange
--     (<>) Red Yellow = Orange
--     (<>) a b = if a == b
--                then a
--                else Brown
-- Semigroup class type law is that the combination of the types must be associative
-- this means that the order in which you apply <> doesn't matter
-- numbers this means that 1 + (2 + 3) = (1 + 2) + 3
-- but something isn't right with our colors

associativeTest1 = (Green <> Blue) <> Yellow -- Brown
associativeTest2 = Green <> (Blue <> Yellow) -- Green

-- guards example
howMuch :: Int -> String
howMuch n
    | n > 10 = "a whole bunch"
    | n > 0 = "not much"
    | otherwise = "we're in debt!"

-- making color associative

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | a == Black || b == Black = Black
             | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange
             | otherwise = Brown

-- composing with identity: Monoids
-- logical definition of Monoid
-- class Semigroup a => Monoid a where
--     identity :: a

-- but for historical reasons, the Monoid typeclass came before the Semigroup typclass and is definied as
class MyMonoid a where
    myMempty :: a
    myMappend :: a -> a -> a
    myMconcat :: [a] -> a

-- most common monoid is a list
-- [] for identity for lists
-- ++ for append operator is just the <> operator for lists

appendExample1 = [1,2,3] ++ []
appendExample2 = [1,2,3] <> []
appendExample3 = [1,2,3] `mappend` mempty

-- quick check 17.4: If you implement `mappend`/`<>` for Integer as * instead of plus, what will your mempty value be?
-- 1 because x * 1 = x

-- mconcat
example = mconcat ["does", "this", "make", "sense?"]
-- mconcat = foldr mappend mempty

-- monoid laws
-- 1. mappend mempty x is x
-- example: [] ++ [1,2,3] = [1,2,3]

-- 2. mappend x mempty is x
-- example: [1,2,3] ++ [] = [1,2,3]

-- 3. mappend x (mappend y z) = mappend (mappend x y) z. note: same as the semigroup law, so you can use <> as mappend if already implemented as an instance of semigroup
-- [1] ++ ([2] ++ [3]) == ([1] ++ [2]) ++ 3

-- 4. mconcat = foldr mappend mempty

-- practical Monoids
-- type Events = [String]
-- type Probs = [Double]

data PTable = PTable Events Probs

instance Show PTable where
    show (PTable (Events events) (Probs probs)) = mconcat pairs
        where pairs = zipWith showPair events probs

createPTable :: [String] -> [Double] -> PTable
createPTable events probs = PTable (Events events) (Probs normalizedProbs)
    where totalProbs = sum probs
          normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

-- cartesian product
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1   -- maps l1 and makes nToAdd copies of the element
          newL1 = mconcat repeatedL1 -- preceding line leave you with a list of lists and you need a way to join them
          cycledL2 = cycle l2 -- by cycling the second list, you can use zipWith to combine these two lists

combineEvents :: [String] -> [String] -> [String]
combineEvents e1 e2 = cartCombine combiner e1 e2
    where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: [Double] -> [Double] -> [Double]
combineProbs p1 p2 = cartCombine (*) p1 p2 -- when combining probabilties, you multiple them

instance Semigroup PTable where
    (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
    (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
    (<>) (PTable (Events e1) (Probs p1)) (PTable (Events e2)  (Probs p2)) = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs = combineProbs p1 p2
instance Monoid PTable where
    mempty = PTable (Events [])  (Probs [])
    mappend = (<>)

-- example PTables. coin and spinner
coin :: PTable
coin = createPTable ["Head", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

probabilityCoinAndSpinner = coin <> spinner
probabilityThreeFlips = mconcat [coin,coin,coin]

-- Q17.1: current implementation of Color doesn't contain an identity element. Modify the code in this unit so that Color does have an identity element, then make Color an instance of Monoid
-- see updated Color above to add Black
instance Monoid Color where
    mempty = Black
    mappend = (<>)

-- Q17.2: If you Events and Probs types were data types and not just synonyms, you could make them instances of Semigorup and Monooid
-- where combineEvents and combineProbs were the <> in each case. Refactor these type sand make instance of Semigroup and Monoid
data Events = Events [String]
data Probs = Probs [Double]

instance Semigroup Events where
    (<>) (Events e1) (Events e2) = Events $ combineEvents e1 e2

instance Semigroup Probs where
    (<>) (Probs p1) (Probs p2) = Probs $ combineProbs p1 p2

instance Monoid Events where
    mempty = Events []
    mappend = (<>)

instance Monoid Probs where
    mempty = Probs []
    mappend = (<>)