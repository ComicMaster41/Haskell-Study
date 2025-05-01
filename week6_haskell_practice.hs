-- Helper Functions, Index Tracking, and Cleanup
-- Count evens between two indices (1-based)
countEvenInRange :: Integral a => a -> a -> [a] -> a
countEvenInRange start end = helper 0 0
  where 
    helper _ count [] = count
    helper idx count (x:xs)
      | (idx + 1) < start = helper (idx + 1) count xs
      | (idx + 1) > end   = count
      | even x            = helper (idx + 1) (count + 1) xs
      | otherwise         = helper (idx + 1) count xs

-- Replace all negative numbers in a list with 0
replaceNegatives :: (Num a, Ord a) => [a] -> [a]
replaceNegatives [] = []
replaceNegatives (x:xs)
  | x < 0     = 0 : replaceNegatives xs
  | otherwise = x : replaceNegatives xs

-- Find the maximum element and its index
maxWithIndex :: (Ord a) => [a] -> (a, Int)
maxWithIndex [] = error "Empty list"
maxWithIndex xs = helper 0 (head xs) 0 xs
  where
    helper _ maxVal maxIdx [] = (maxVal, maxIdx)
    helper idx maxVal maxIdx (x:xs)
      | x > maxVal = helper (idx + 1) x idx xs
      | otherwise  = helper (idx + 1) maxVal maxIdx xs
