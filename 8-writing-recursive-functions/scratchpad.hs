-- consider this, write your down drop function

take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

drop' n xs | n <= 0 = xs
drop' _ [] = []
-- drop' n xs = reverse (take (length xs - n) (reverse xs))
drop' n (_:xs) = drop' (n - 1) xs

-- implementing length
length' [] = 0
length' (_:xs) = 1 + length' xs

-- implementing cycle (make a list and repeat it forever)
cycle' (x:xs) = x:cycle' (xs ++[x])


-- ackerman function
-- if m=0, return n + 1
-- if n=0, return A(m-1, 1)
-- if both m != 0 and n != 0, then A(m-1, A(m, n-1))
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m-1) 1
ackerman m n = ackerman (m-1) (ackerman m (n-1))

-- the collatz conjecture
-- if n is 1, you're finished
-- if n is even, repeat with n/2
-- if n is odd, repeat with n * 3 + 1
collatz 1 = 1
collatz n | even n = 1 + collatz (n `div` 2)
collatz n = 1 + collatz (n*3+1)


-- Q8.1 - implement your own version of reverse, which reverses a list
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Q8.2 - calculating fibonacci numbers is perhaps the single most common example of a recursive function. the straightForward definition is as follows
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- write a function, fastFib, that can compute the 1000th fibonacci number nearly instantly
-- hint: fastFib takes 3 arguements, n1, n2, and counter. to calculat the 1000th fibonacci number, you call fastfib 1 1 1000 and for the 5th you call fastFib 1 1 5
fastFib n1 _ 1 = n1
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)