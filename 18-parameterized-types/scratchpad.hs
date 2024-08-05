import qualified Data.Map as Map

-- types that take arguements
data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- quick check 18.1: What's the type of wrap (Box 'a') -> Box (Box Char)

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double
aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String
aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

type Initials = Triple Char
initials :: Initials
initials = Triple 'H' 'P' 'L'

-- accessors for the Triple type
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- quick check 18.2: What's the difference between tarnsform and the map function for lists?
-- Transform function doesn't allow you to change type (must go from a -> a) where the map function can go from (a -> b) and change types

-- Most common parameterized type is a List
-- data [] a = [] | a:[a]
-- : is the cons operator is a data constructor
-- [] is a built in type and data constructor for a list

data List a = Empty | Cons a (List a) deriving Show

builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 (Empty)))

builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' (Empty)))

ourMap:: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (Cons a rest) = Cons (f a) (ourMap f rest)

-- 18.2 types with more than one parameter
-- tuples

-- two tuple definition
-- data (,) a b = (,) a b
itemCount1 :: (String,Int)
itemCount1 = ("Erasers", 25)

itemCount2 :: (String,Int)
itemCount2 = ("Pencils", 25)

itemCount3 :: (String,Int)
itemCount3 = ("Pens", 13)

itemInventory :: [(String,Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]

-- quick check 18.3: What would happen if you tried to add ("Paper", 12.4) to our inventory?
-- Error due to our inventory type of (String,Int) and the new value is (String,Double)

-- quick check 18.4: What is the kind of (,,)
-- * -> * -> * -> * or (a,b,c)

-- data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Q18.1: For the types `Triple` and `Box`, implement a function similar to `map`, `tripleMap`, and `boxMap
tripleMap :: (a -> b) -> [Triple a] -> [Triple b]
tripleMap _ [] = []
tripleMap f (Triple x y z : rest) = Triple (f x) (f y) (f z) : tripleMap f rest

boxMap :: (a -> b) -> [Box a] -> [Box b]
boxMap _ [] = []
boxMap f (Box a : rest) = Box (f a) : boxMap f rest

-- Q18.2: Modify the Organ type so it can be used as a key. Then build a Map, organInventory, of each organ to its count in the organCatalog
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

organInventory :: Map.Map Organ Int
organInventory = Map.fromListWith (+) [(organ, 1) | organ <- organs]