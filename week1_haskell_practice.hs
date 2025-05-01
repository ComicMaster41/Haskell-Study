-- Types and Simple Functions

x :: Integer
x = 10

addThree :: Num a => a -> a
addThree a = a + 3

factorialBig :: Integer -> Integer
factorialBig n = if (n == 0) then 1 else n * factorialBig (n - 1)

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide a d = Just (a / d)

addWithBind = Just 5 >>= \x -> Just (x + 3)

doubleIfEqual :: (Eq a, Num a) => a -> a -> a
doubleIfEqual a b = if (a == b) then a * 2 else a

squareList :: Num a => [a] -> [a]
squareList xs = [x^2 | x <- xs]

listPractice :: (Integral a) => [a] -> [a]
listPractice xs = [x^2 | x <- [1..10], even x]

wolf = [(x, y) | x <- [1, 2, 3], y <- [4, 5]]
bear = [x | x <- [1..30], x `mod` 3 == 0, x `mod` 5 == 0]
fruit = [length x | x <- ["cat", "apple", "banana"]]

signLabel :: (Num a, Ord a) => a -> String
signLabel x
    | x < 0 = "Negative"
    | x > 0 = "Positive"
    | otherwise = "Zero"

main :: IO ()
main = do
    print (squareList [1, 2, 3])
    print (signLabel 12)

list_comp = [x * 3 | x <- [1..20], odd x]
