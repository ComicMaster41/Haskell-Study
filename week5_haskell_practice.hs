-- Interval Products, Ranges, and Helper Functions
-- Interval product using recursion (1-indexed)
intervalProduct :: (Eq a, Num a) => a -> a -> [a] -> a
intervalProduct _ _ [] = 1
intervalProduct start end (x:xs)
    | start == 1 && end == 1 = x
    | start == 1 = x * intervalProduct 1 (end - 1) xs
    | otherwise  = intervalProduct (start - 1) (end - 1) xs

-- Multiples of n up to a limit
multiplesUpTo :: (Integral a) => a -> a -> [a]
multiplesUpTo n limit = [x | x <- [1..limit], mod x n == 0]

-- Shared elements between two lists
sharedElements :: (Eq a) => [a] -> [a] -> [a]
sharedElements lst1 lst2 = [x | x <- lst1, x `elem` lst2]

-- Perfect squares up to n
perfectSquaresUpTo :: (Ord a, Num a, Enum a) => a -> [a]
perfectSquaresUpTo sqr = [x^2 | x <- [1..sqr], x^2 <= sqr]

filterPairsByProduct :: (Num a, Ord a) => [a] -> [a] -> a -> [(a, a)]
filterPairsByProduct i j n = [(x, y) | x <- i, y <- j, x * y > n]

sumDiagonal :: [[Int]] -> Int
sumDiagonal mtx = sum [row !! i | (i, row) <- zip [0..] mtx]

listOfFactors :: [Int] -> [[Int]]
listOfFactors lst = [ [f | f <- [2..(x-1)], mod x f == 0] | x <- lst ]
