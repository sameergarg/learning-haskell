-- my length
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- take implementation
myTake 0 _ = []
myTake _ []     = []
myTake n (x:xs) = x : rest where rest = myTake (n-1) xs

-- drop implementation
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (x:xs) = myDrop (n-1) xs

-- cycle implementation
-- take 5 (cycle [1..3])
-- [1,2,3,1,2]

myCycle (first:rest) = first : myCycle (rest ++ [first])

-- Ackermann function
-- “A(m, n) to save space. The Ackermann function follows these three rules:
   --
   --If m = 0, return n + 1.
   --If n = 0, then A(m – 1, 1).
   --If both m != 0 and n != 0, then A(m –1, A(m, n – 1)).”
   --
   --Excerpt From: Will Kurt. “Get Programming with Haskell.” iBooks.
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann(m-1) (ackermann m (n-1))

-- collatz conjencture
-- “Collatz conjecture involves defining a recursive process given a starting number n:
   --
   --If n is 1, you’re finished.
   --If n is even, repeat with n/2.
   --If n is odd, repeat with n × 3 + 1”
   --
   --Excerpt From: Will Kurt. “Get Programming with Haskell.” iBooks.
collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n*3 + 1)

-- 8.1 reverse
myreverse [] = []
myreverse (x:xs) = myreverse(xs) ++ [x]

-- 8.2 Fibonacci
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--fibFast n1: will memoize number at counter position,  n2:will memoize number at counter-1 position, counter: required position which reduces to 0
fibFast n1 _ 1 = n1
fibFast _ n2 2 = n2
fibFast n1 n2 3 = n1 + n2
fibFast n1 n2 counter = fibFast (n1 + n2) n1 (counter - 1)

--fibFast 1 1 5
--fibFast (1+1) 1 4
--fibFast (2+1) 2 3

