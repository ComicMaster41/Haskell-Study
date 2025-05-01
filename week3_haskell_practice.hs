-- Factors of x (excluding 1 and x)
x = 12
factors = [y | y <- [2..(x - 1)], mod x y == 0]

-- Cartesian product of two lists
cart = [(x, y) | x <- [1, 2], y <- ['a', 'b']]

-- All pairs (x,y) such that x + y > 7
greaterPairs = [(x, y) | x <- [1,2,3], y <- [4,5,6], x + y > 7]

-- Type signature for a satisfy function
satisfy :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
satisfy test xs ys = [(x, y) | x <- xs, y <- ys, test x y]

sumListRec :: Num a => [a] -> a
sumListRec [] = 0
sumListRec (x:xs) = x + sumListRec xs

removeEvens :: [Int] -> [Int]
removeEvens [] = []
removeEvens (x:xs)
    | mod x 2 == 0 = removeEvens xs
    | otherwise    = x : removeEvens xs

divisors :: Integer -> [Integer]
divisors 0 = []
divisors n = [x | x <- [1..n], mod n x == 0]

commonDivisors :: Integer -> Integer -> [Integer]
commonDivisors a b = [x | x <- divisors a, x `elem` divisors b]
