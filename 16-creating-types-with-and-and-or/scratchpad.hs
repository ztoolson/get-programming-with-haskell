-- consider this

data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving Show
data BreakfastMeat = Sausage | Bacon | Ham deriving Show
data BreakfastMain = Egg | Pancake | Waffle deriving Show

-- you want to create a Breakfast special
-- kids breakfast - on main and one side
data KidsBreakfastSpecial = KidsBreakfastSpecial BreakfastSide BreakfastMain
-- basic breakfast - one main, one meat, and one side
data BasicBreakfastSpecial = BasicBreakfastSpecial BreakfastMain BreakfastMeat BreakfastSide
-- the lumberjack - two mains, two meats, and three sides
data LumberjackBreakfastSpecial = LumberjackBreakfastSpecial 
    BreakfastMain BreakfastMain
    BreakfastMeat BreakfastMeat
    BreakfastSide BreakfastSide BreakfastSide

-- product types - combining types with "and"
-- think of c structs. these are by default "and" types
-- data AuthorName = AuthorName String String
-- data Book = Author String String Int

-- Quick check 16.1 - Rewrite AuthorName by using record syntax
data AuthorName = AuthorName {
      firstName :: String
    , lastName :: String
}
-- data Book = Book {
--       author :: AuthorName
--     , isbn   :: String
--     , title  :: String
--     , year   :: Int
--     , price  :: Double
-- }

-- common sum type
data Bool = False | True

type FirstName = String
type LastName = String
type MiddleName = String

-- using a sum type to model names with and without middle names
data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName
    | FirstNameWIthTwoInits FirstName Char Char deriving Show

-- a creator type that's either an author or an artist
data Creator = AuthorCreator Author | ArtistCreator Artist deriving Show
data Author = Author Name deriving Show
data Artist = Person Name | Band String deriving Show

hpLovecraft :: Creator
hpLovecraft = AuthorCreator
                (Author
                    (TwoInitialsWithLast 'H' 'P' "lovecraft"))

-- Book type using Creator
data Book = Book {
      author :: Creator
    , isbn   :: String
    , title  :: String
    , year   :: Int
    , bookPrice  :: Double
}

data VinylRecord = VinylRecord {
      artist      :: Creator
    , recordTitle :: String
    , recordYear  :: Int
    , recordPrice :: Double
}

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | FreePamphlet Pamphlet

-- Add collectabletoy type
data CollectibleToy = CollectivleToy {
      name        :: String
    , description :: String
    , toyPrice    :: Double
}

price :: StoreItem -> Double
price (BookItem book)     = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy)       = toyPrice toy
price _       = 0.0

-- Quick check 16.3: Assume that Creator is an instance of Show. WRite a `madeBy` function that has the type
-- StoreItem -> String and does its best to determine who made the StoreItem
madeBy :: StoreItem -> String
madeBy (BookItem book) = show $ author book
madeBy (RecordItem record) = show $ artist record
madeBy _ = "unknown"

-- Q16.1: To further complicate the items in yoru store, you eventually keep an inventory
-- of free pamphlets. Pamphlets have a title, description, and a contact field for the organization
-- that provides the pamphlet. Create the `Pamphlet` type and add it to StoreItem. Additionally, modify
-- price so that it works with Pamphlet

data Pamphlet = Pamphlet {
      pampTitle       :: String
    , pampDescription :: String
    , contact         :: String
} deriving Show

-- Q16.2: Create a Shape type that includes the following shapes: Circle, Square, and Rectangle.
-- Then write a function to compute the perimeter of a Shape as well as its area

type Radius = Double
type Length = Double
type Width = Double
data Shape = Circle Radius | Square Length | Rectangle Width Length deriving Show

perimeter :: Shape -> Double
perimeter (Circle radius) = 2 * pi * radius
perimeter (Square length) = 4 * length
perimeter (Rectangle width length) = 2 * width + 2 * length

area :: Shape -> Double
area (Circle radius) = pi * radius * radius
area (Square length) = length * length
area (Rectangle width length) = width * length