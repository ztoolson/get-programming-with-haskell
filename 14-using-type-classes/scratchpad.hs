data SixSideDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Ord, Eq, Enum)

-- Creating an instance of Show for SizSideDie
-- instance Show SixSideDie where
--     show S1 = "one"
--     show S2 = "two"
--     show S3 = "three"
--     show S4 = "four"
--     show S5 = "five"
--     show S6 = "siz"

-- quick check. Rewrite this definition to print the numerals 1-6 instead
instance Show SixSideDie where
    show S1 = "1"
    show S2 = "2"
    show S3 = "3"
    show S4 = "4"
    show S5 = "5"
    show S6 = "6"

-- Defult implementation and minimum complete definitions

--implementing an instance of Eq for SizSideDie
-- instance Eq SixSideDie where
--     (==) S6 S6 = True
--     (==) S5 S5 = True
--     (==) S4 S4 = True
--     (==) S3 S3 = True
--     (==) S2 S2 = True
--     (==) S1 S1 = True
--     (==) _ _ = False

-- Quick Check 14.2: Use Hoogle to search for the `RealFrac` type class. What's its minimal complete definition?
-- properFraction

-- implementing ord
-- instance Ord SixSideDie where
--     compare S6 S6 = EQ
--     compare S6 _ = GT
--     compare _ S6 = LT
--     compare S5 S5 = EQ
--     compare S5 _ = GT
--     compare _ S5 = LT
--     compare S4 S4 = EQ
--     compare S4 _ = GT
--     compare _ S4 = LT
    --- ... lots of comparisons if you want to complete

data Test1 = AA | ZZ deriving (Eq, Ord)
data Test2 = ZZZ | AAA deriving (Eq, Ord)


-- implementing Enum for SizSideDie (errors with implementation)
-- instance Enum SixSideDie where
--     toEnum 0 = S1
--     toEnum 1 = S2
--     toEnum 2 = S3
--     toEnum 3 = S4
--     toEnum 4 = S5
--     toEnum 5 = S6
--     toEnum _ = error "No such value" -- problem is where when trying to define an infinite list
-- 
--     fromEnum S1 = 0
--     fromEnum S2 = 1
--     fromEnum S3 = 2
--     fromEnum S4 = 3
--     fromEnum S5 = 4
--     fromEnum S6 = 5

-- Type classses for more-complex types
-- type Name = (String, String)
names :: [Name]
names = [ Name ("Emil", "Cioran")
        , Name ("Eugene", "Thacker")
        , Name ("Friedrich", "Nietzsche") ]

-- Attempt to implement Ord for type synonym
-- cannot do this with a type synonym since haskell already knows how to sort tuple
-- instance Ord Name where
--     compare (f1, l1) (f2, l2) = compare (l1,f1) (l2,f2)

data Name = Name (String, String) deriving (Show, Eq)
instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1,f1) (l2,f2)

-- data Name = Name (String, String) deriving (Show, Eq)
-- instead of the above defition, you can use newtype which works with ony one type constructor and one type
-- generally it is better

-- Q14.1: Ignoring the fact that you can easily use `deriving` for `Eq` and `Ord`, use the derived implementation of Enum to make manually defining Eq and Ord much easier
data Test = A | B | C deriving Enum
instance Eq Test where
    (==) x y = fromEnum x == fromEnum y

instance Ord Test where
    compare x y = compare (fromEnum x) (fromEnum y)

-- Q14.2: Define a five-sided die (FiveSidedDie type). Then define a type class named Die and at least one method that would be useful to have for a Die
data FiveSideDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving Eq

class Eq a => Die a where
    roll :: a -> Int

instance Die FiveSideDie where
    roll Side5 = 5
    roll Side4 = 4
    roll Side3 = 3
    roll Side2 = 2
    roll Side1 = 1