main = print (calcChange 5 20)

calcChange owed given = if given - owed > 0
    then given - owed
    else 0
