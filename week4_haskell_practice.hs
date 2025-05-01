--  Filtering, Mapping, Folding, and Matrix Sums

sumSquaresBelow :: Integer -> Integer -> Integer
sumSquaresBelow [] _ = 0
sumSquaresBelow _ 0 = 0
sumSquaresBelow xs thres = foldr (+) 0 (map (^2) (filter (< thres) xs))

rowSums :: [[Integer]] -> [Integer]
rowSums [[]] = []
rowSums xss = map (foldr (+) 0) xss

xs = [x^2 | x <- [1..5]]

pairList = [(x, y) | x <- [1,2,3], y <- [4,5]]

pairSumOver6 = [(x, y) | x <- [1,2,3], y <- [4,5], x + y > 6]

-- Manual transpose using recursion
myTranspose2 :: [[Int]] -> [[Int]]
myTranspose2 [[]] = [[]]
myTranspose2 ([]:_) = [[]]
myTranspose2 xss = map head xss : myTranspose2 (map tail xss)

firstElements :: [[Int]] -> [Int]
firstElements xss = map head xss

restElements :: [[Int]] -> [[Int]]
restElements xss = map tail xss

sumFirstElements :: [[Int]] -> Int
sumFirstElements ([]:_) = 0
sumFirstElements xss = sum (map head xss)
