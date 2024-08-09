import qualified Data.Map as Map

-- run this program like this
-- ghc pizza.hs
-- ./pizza
-- What is the size of pizza 1
-- <enter input> Example: 12
-- What is the cost of pizza 1
-- 15
-- What is the size of pizza 2
-- 18
-- What is the cost of pizza 2
-- 20

main :: IO()
main = do
    putStrLn "What is the size of pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine

    putStrLn "What is the size of pizza 2"
    size2 <- getLine
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine

    let pizza1 = Pizza (read size1) (read cost1)
    let pizza2 = Pizza (read size2) (read cost2)

    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)


-- areaGivenDiameter calculate area of pizza (circle) given a diameter
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

-- actually i don't like the double double, i want to name the fields
-- type Pizza = (Double, Double)
data Pizza = Pizza {
      size :: Double
    , cost :: Double
} deriving (Show, Eq)

-- costPerInch calculates the cost for the area of a pizza
costPerInch :: Pizza -> Double
costPerInch (Pizza size cost) = cost / areaGivenDiameter size

-- compareTwoPizzas to determine which one is the better deal
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
        if costP1 < costP2
        then p1
        else p2
    where costP1 = costPerInch p1
          costP2 = costPerInch p2

-- describePizza will say which pizza is cheaper at the size and price per square inch
describePizza :: Pizza -> String
describePizza (Pizza size cost) = "The " ++ show size ++ " pizza" ++
        " is cheaper at " ++ show costSqInch ++ " per square inch"
    where costSqInch = costPerInch (Pizza size cost)


-- costData is a map representing pizza cost. key = pizza # and value = cost.
costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

-- sizeData is a map
-- sizeData is a map representing pizza size. key = pizza # and value = size.
sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]

maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData

    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData

    let pizza1 = Pizza size1 cost1
    let pizza2 = Pizza size2 cost2

    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

