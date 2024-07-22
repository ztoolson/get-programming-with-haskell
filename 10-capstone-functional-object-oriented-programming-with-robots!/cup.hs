-- model a cup of coffee. an object with one property

-- use a closure to store the field
-- creating a constructor
cup flOz = \message -> message flOz

-- adding accessors to your object
getOz aCup = aCup (\flOz -> flOz)

-- updating state of flOz property on the cup
drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
    where flOz = getOz aCup
          ozDiff = flOz - ozDrank

-- helper to tell if the cup is empty
isEmpty aCup = getOz aCup == 0

-- aCup = cup 6
-- coffeeCup = cup 12

-- getOz coffeeCup
-- 12


-- afterASip = drink coffeeCup 1
-- getOz afterASip
-- 11
-- afterTwoSips = drink afterASip 1
-- getOz afterTwoSip
-- 10
-- afterGulp = drink afterTwoSips 4
-- getOz afterGulp
-- 6

-- afterBigGulp = drink coffeeCup 20
-- getOz afterBigGulp
-- 0


-- afterManySips = foldl drink coffeeCup [1,1,1,1,1]
-- getOz afterManySips
-- 7