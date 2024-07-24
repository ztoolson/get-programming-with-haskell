-- Using type classes: addThenDouble

addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) *2

-- Defining your own type class: Describable
class Describable a where
    describe :: a -> String

-- defining the Icecream type
data Icecream = Chocolate | Vanilla -- note identical to Bool but doesn't implement show 
data Icecream' = Choc | Van deriving (Show, Eq, Ord)

-- quick check 13.3 see which flavor Haskell thinks is superior by deriving the Ord type class
-- Van > Choc == True

-- Q13.3: Write the following function that works just like succ on Bounded types but can be called an unlimted
-- number of times without error.

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
    then minBound
    else succ n

cycleSuccGuard :: (Bounded a, Enum a, Eq a) => a -> a
cycleSuccGuard n
    | n == maxBound = minBound
    | otherwise     = succ n