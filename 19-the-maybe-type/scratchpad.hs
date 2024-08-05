import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- definition of Maybe
-- data Maybe a = Nothing | Just a

lookup :: Maybe Organ
lookup = Map.lookup 13 organCatalog -- Just Brain

lookupNotFound :: Maybe Organ
lookupNotFound = Map.lookup 6 organCatalog -- Nothing

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = 
    length (filter (\x -> x == Just organ) available)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

isNothing :: Maybe Organ -> Bool
isNothing Nothing = True
isNothing (Just _) = False

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

-- quick check 19.2: write a function `numOrZero` that takes a Maybe Int and return 0 if it's nothing otherwise returns a value
numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just x) = x

-- defining key functions and data types for mad scientist request
data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

-- proces and report
process:: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location,Container) -> String
report (location,container) = show container ++ " in the " ++ show location

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
    where organ = Map.lookup id catalog

-- quick check 19.3: How would you rewrite `report` so that it works with Maybe (Location, Container)
-- and handles the case of the missing Organ
reportCheck :: Maybe (Location,Container) -> String
reportCheck Nothing = "missing organ"
reportCheck (Just (location,container)) = show container ++ " in the " ++ show location

-- Q19.1: Write a function `emptyDrawers` that takes the output of getDrawerContents and tells you
-- the number of drawers that are empty
-- my answer
numOfDrawersEmpty :: Int
numOfDrawersEmpty = length $ filter isNothing (getDrawerContents possibleDrawers organCatalog)

-- ai answer
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers = length . filter (== Nothing)

emptyDrawerCount = emptyDrawers availableOrgans

-- Q 19.2: Write a version of map that works for Maybe types called `maybeMap`
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)