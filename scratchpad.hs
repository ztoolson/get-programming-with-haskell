fac :: Int -> Int
fac n = product [1..n]

-- freshman haskell programmer
fac' n = if n == 0
    then 1
    else n * fac (n-1)

-- another junior haskell prgrammer that read n + k patterns are "a disgusting part of haskell"
fac'' 0 = 1
fac'' n = n * fac'' (n-1)

-- senior haskell programmer (leans right)
facFoldR n = foldr (*) 1 [1..n]

-- senior haskell programmer (leans left)
facFoldL n = foldl (*) 1 [1..n]


-- write using guard clauses
facGuard n  
    | n == 0 = 1
    | n > 0 = n * facGuard (n-1)
    | otherwise = error "Factorial is not defined for negative numbers"


product' :: Num a => [a] -> a -- for any numberic type a, then product' takes a list of a and will return a single a thing
product' [] = 1
product' (n:ns) = n * product' ns