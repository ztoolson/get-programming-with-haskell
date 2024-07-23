import Data.List

ifEvenInc n = if even n
              then n + 1
              else n

ifEvenDouble n = if even n
                 then n * 2
                 else n

ifEvenSquare n = if even n
                 then n^2
                 else n


-- instead of doing custom, since the functions are almost identical pass in a function as a param
ifEven myFunction x = if even x
    then myFunction x
    else x

inc n = n + 1
double n = 2 * n
square n = n^2

ifEvenInc' n = ifEven inc n
ifEvenDouble' n = ifEven double n
ifEvenSquare' n = ifEven square n

-- quick check. write a function for cubing x and pass it to ifEven
ifEvenCube n = ifEven (\x -> x*x*x) n


-- 4.4 custom sorting names
names = [("Ian", "Curtis"),
    ("Bernard", "Summer"),
    ("Peter", "Hook"),
    ("Stephen", "Morris"),
    ("Bob", "Morris")]

compareLastNames name1 name2 = if lastName1 > lastName2
    then GT
    else if lastName1 < lastName2
        then LT
        else EQ
    where lastName1 = snd name1
          lastName2 = snd name2

-- linter suggested to use guards compared to what is in the book
compareLastNames' name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | otherwise = EQ
  where
      lastName1 = snd name1
      lastName2 = snd name2

-- adjustCompareLastNames to then compare first names if the last names are EQ
compareLastNamesThenFirst name1 name2 = if lastName1 > lastName2
    then GT
    else if lastName1 < lastName2
        then LT
        else if firstName1 > firstName2
            then GT
            else if firstName1 < firstName2
                then LT
                else EQ
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2

-- apply lint suggestion to use guards
compareLastNamesThenFirst' name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | firstName1 > firstName2 = GT
  | firstName1 < firstName2 = LT
  | otherwise = EQ
  where
      lastName1 = snd name1
      lastName2 = snd name2
      firstName1 = fst name1
      firstName2 = fst name2
useCompareNames = print $ sortBy compareLastNames names


-- addressLetter
addressLetter name location = locationFunction name
    where locationFunction = getLocationFunc location

sfOffice name = if lastName < "L"
    then nameText
        ++ " - P.O. Box 1234 - San Francisco, CA 94111"
    else nameText
        ++ " - P.O. Box 1010 - San Francisco, CA 94109"
    where lastName = snd name
          nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": P.O. Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - P.O. Box 456 - Reno , NV, 84523"
    where nameText = fst name ++ " " ++ snd name

getLocationFunc location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> fst name ++  " " ++ snd name)

-- Q 4.1
-- anything that can be compared in haskell (for example [Char]) can be compared with a function called compare
-- rewrite the compare last names by using compare
compareLastNames'' name1 name2 = compare lastName1 lastName2
    where lastName1 = snd name1
          lastName2 = snd name2

-- Q 4.2 Define a new function for Washington D.C. and add it to the getLocationFunc
dcOffice name = nameText ++ " - 1600 Pennsylvania Avenue NW, Washington, DC 20500"
    where nameText = fst name ++ " " ++ snd name ++ " ESQ"