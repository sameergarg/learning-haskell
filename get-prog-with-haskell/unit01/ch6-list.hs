import Data.List

backwardsInfinity = reverse [1..]

-- 6.1
myRepeat v = cycle [v]

-- 6.2
subseq start end list = end `take` drop start list

-- 6.3
ex e list = elemIndex e list

inFirstHalf x xs = x `elem` firstHalf
                   where firstHalf = take half xs
                         half = length xs `div` 2