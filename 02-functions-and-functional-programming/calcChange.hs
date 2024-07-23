main = print (calcChange 5 20)

calcChange owed given = if change > 0
    then change
    else 0
    where change = given - owed

-- exercises

inc x = x + 1
double x = x * 2
square x = x * x

f n = if isEven then n - 2 else 3 * n + 1
    where isEven = even n