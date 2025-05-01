addPairs :: Num a => [a] -> [a] -> [a]
addPairs [] _ = []
addPairs _ [] = []
addPairs a b = zipWith (+) a b

concatStrings :: [String] -> String
concatStrings a = foldr (++) "" a

addFive = map (+5) [10, 20, 30]

filterLarge :: (Ord a) => [a] -> [a]
filterLarge a = filter (> 100) a

multiplyLists :: (Num a) => [a] -> [a] -> [a]
multiplyLists a b = zipWith (*) a b

sumList :: Num a => [a] -> a
sumList k = foldr (+) 0 k

addOne :: Num a => [a] -> [a]
addOne xs = map (\x -> x + 1) xs

pairMultiply = \(x, y) -> x * y

sumOddSquares = foldr (+) 0 $ map (^2) $ filter odd [1..20]

import Data.List (transpose)

pairZip = zip [1, 2, 3] ['a','b','c']

zipMult = zipWith (*) [2, 4, 6] [1, 3, 5]

myTranspose :: [[Int]] -> [[Int]]
myTranspose [] = []
myTranspose ([]:_) = []
myTranspose x = map head x : myTranspose (map tail x)

dotProduct = sum (zipWith (*) [1,2,3] [4,5,6])

matA = [[1,2], [3,4]]
matB = [[5,6], [7,8]]
matrixProduct = map (\row -> map (\col -> sum (zipWith (*) row col)) (transpose matB)) matA
