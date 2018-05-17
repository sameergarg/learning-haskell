-- approach 1

sumSquareOrSquareSum x y = if sumSquare > squareSum then sumSquare else squareSum
                           where sumSquare = (x + y)^2
                                 squareSum = x^2 + y^2

-- approach 2. Using helper function

body sumSquare squareSum = if sumSquare > squareSum then sumSquare else squareSum

sumSqOrSqSumUsingHelper x y = body ((x + y)^2) (x^2 + y^2)


-- approach 3. Using Lambdaymous function

sumSqOrSqSumUsingLambdaFunc x y = (\sumSquare squareSum ->
                                if sumSquare > squareSum
                                then sumSquare
                                else squareSum)
                                ((x + y)^2) (x^2 + y^2)

dubsdubs x = dubs * 2 where dubs = x*2

-- same as above using Lambdaymous function
dubsDubsUsingLambda x = (\dubs -> dubs*2) 2*x




-- using let instead of where
dubsDubsUsingLet x = let dubs = 2*x
                   in 2*dubs

sumSqOrSqSumUsingLet x y = let sumSquare = (x^2 + y^2)
                               squareSum = (x + y)^2
                           in
                            if sumSquare > squareSum
                            then sumSquare
                            else squareSum

overwrite = let x = 2
            in
              let x = 3
            in
              let x = 4
            in
              x

overwriteUsingLambdas = (\x ->
                          (\x ->
                            (\x -> x) 4
                          ) 3
                        ) 2

counter x = (\x -> (\x -> x + 1) x + 1) x