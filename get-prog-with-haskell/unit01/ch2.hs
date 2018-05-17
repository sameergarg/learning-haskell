calcChange owed given = if change > 0
                        then  change
                        else 0
                        where change = given - owed

doublePlus2 x = doublex + 2 where doublex = 2*x

inc x = x+1

double x = x*2

square x = x*x

changeN n = if even(n) then n-2 else 3*n + 1

changeN2 n = if mod n 2 ==0 then n-2 else 3*n + 1