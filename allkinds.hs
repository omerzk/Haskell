gcd' :: (Integral a) => a -> a -> a
-- find the greatest common denominator of two numbers a,b recursively
gcd' m n 
	| n == 0 = m
	| otherwise = gcd n (m `mod` n)

--------------------------------------------------------------------------
insertIn :: Int -> b -> [b] -> [b] 

insertIn i e xs
  | i == 0 = e:xs 
  | i > length(xs) = []
  | otherwise = head xs : (insertIn (i-1) e (tail xs))
---------------------------------------------------------------------------
lastNitems :: Int -> [a] -> [a]

lastNitems n xs
    | n > length xs = []
    | n == length xs = xs
    | otherwise = lastNitems n (tail xs)
--------------------------------------------------------------------------
lastNitems' n xs = take n (reverse xs)
---------------------------------------------------------------------------
slice :: Int -> [a]-> ([a], [a])

slice k xs
    | k < 0 = ([],xs) 
    | k > length xs = (xs,[]) 
    | otherwise = ((take k xs), lastNitems ((length xs) - k) xs)

--------------------------------------------------------------------------
runLength :: (Eq a) => [a] -> [(a, Int)]


runLength [] = []
runLength sq = (head sq, (length sq) - (length r)) : runLength r 
    where r = run sq
          run :: (Eq a) => [a] -> [a]
          run (x:xs)
            | xs == [] = []
            | x == (head xs) = run xs 
            | otherwise = xs 
----------------------------------------------------------------------------
subSets :: (Eq a , Integral b) => b -> [a] -> [[a]]

subSets k set 
   | k > len || k <= 0 = [[]]
   | k == len = [set] 
   | otherwise = ((head set) : subSets (k - 1) (tail set)) ++ (subSets k (tail set))
   where len = length set
choose ::  Int -> [b] -> [[b]]
choose k set 
   | k == 0 || k > len  = [[]]
   |otherwise =  ((head set):) `fmap` (choose (k-1) (tail set)) ++ choose k (tail set)
   where len = length set
-----------------------------------------------------------------------------
--greyCode :: Int -> Int
--greyCode n =
--    (n/2) `xor` n
------------------------------------------------------------------------------

cumSum :: (Integral b) => [b] -> b -> [b]

cumSum [] _ = []
cumSum sq sumSoFar 
    | tail sq == [] = sumSoFar + (head sq):[] 
    | otherwise = sumSoFar + (head sq) : cumSum (tail sq) (sumSoFar + (head sq)) 

-- maxSubSq :: (Ord a) => [a]->[a]

-- maxSubSq xs = maxi 0 1 (length xs) xs
--     where maxi maxSoFar i j xs
--             | xs == [] || j < i = []
--             | j == i  = [xs !! i]
--             | otherwise = maxi  

main = do putStrLn (show (subSets 4 [8,0,0,0,9,9,9,9,9,8,88,8])) 