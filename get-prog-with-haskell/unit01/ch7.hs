-- gcd

gcdRec a b = if rem == 0
          then b
          else gcdRec b rem
          where rem = a `mod` b

-- pattern match
inWords n = case n of
              1 -> "one"
              2 -> "two"
              _ -> "too large"

sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount _ = "too many"

isEmpty [] = True
isEmpty _ = False

myHead (x:xs) = x
myHead [] = error "empty list"

myTail [] = []
myTail (_:xs) = xs

myGCDPattMatch a b = case rem of
                      0 -> b
                      _ ->  myGCDPattMatch b rem
                     where rem = a `mod` b
