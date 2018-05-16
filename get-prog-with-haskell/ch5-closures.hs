inc = (\x -> x + 1)

double = (\x -> 2*x)

applyIfEven fn = (\x -> if even x then fn x else x)

ifEvenFn fn x = if even x then fn x else x

ifEvenDouble x = ifEvenFn double x

getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey

getHostRequestURL host = (\apiKey resource id -> getRequestURL host apiKey resource id)

hostBuilder = (\host -> getHostRequestURL host)

genApiRequestBuilder hostBuilder apiKey resource = (\id ->
                                            hostBuilder apiKey resource id)
apiRequestBuilder host =  genApiRequestBuilder getHostRequestURL host

exampleBuilder = getRequestURL "http://example.com" "1337hAsk3ll" "books"

-- QC5.4
subtract2 = flip (-) 2

-- Q 5.1
ifEvenInc = ifEvenFn inc

binaryPartialApplication binFn = (\x y -> binFn x y)







