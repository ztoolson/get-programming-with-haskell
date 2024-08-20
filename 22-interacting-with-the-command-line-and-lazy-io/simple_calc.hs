-- Q22.1 Write a program, simple_calc.hs, that reads simple equations involving adding two numbers or multiplying two numbers.
-- the program should solve the equation each user types into the line as each line is entered.

main :: IO ()
main = do
  putStr "Equation: "
  equation <- getLine
  let result = calculate equation
  putStrLn ("Answer: " ++ show result)

calculate :: String -> Double
calculate equation = 
  let (num1, op, num2) = parseEquation equation
  in
    case op of
      '+' -> num1 + num2
      '*' -> num1 * num2

parseEquation :: String -> (Double, Char, Double)
parseEquation equation =
  let (num1, rest) = break (`elem` "+*") equation
      op = head rest
      num2 = tail rest
  in (read num1, op, read num2)
