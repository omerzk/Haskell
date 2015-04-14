import Bits
import Int
gcd' :: (Integral a) => a -> a -> a
-- Complexity linear in input size
gcd' m n 
	| n == 0 = m
	| otherwise = gcd n (m `mod` n)

--------------------------------------------------------------------------
insertIn :: Int -> b -> [b] -> [b] 
-- Complexity linear in input size
insertIn i e xs
  | i == 0 = e:xs 
  | i > length(xs) = []
  | otherwise = head xs : (insertIn (i-1) e (tail xs))
---------------------------------------------------------------------------
lastNitems :: Int -> [a] -> [a]
-- Complexity linear in input size
lastNitems n xs
    | n > length xs = []
    | n == length xs = xs
    | otherwise = lastNitems n (tail xs)
--------------------------------------------------------------------------
lastNitems' n xs = take n (reverse xs)
---------------------------------------------------------------------------
slice :: Int -> [a]-> ([a], [a])
-- Complexity linear in input size
slice k xs
    | k < 0 = ([],xs) 
    | k > length xs = (xs,[]) 
    | otherwise = ((take k xs), lastNitems ((length xs) - k) xs)
--------------------------------------------------------------------------
runLength :: (Eq a) => [a] -> [(a, Int)]
-- Complexity linear in input size
runLength [] = []
runLength sq = (head sq, (length sq) - (length r)) : runLength r 
    where run :: (Eq a) => [a] -> [a]
          r = run sq
          run (x:xs)
            | xs == [] = []
            | x == (head xs) = run xs 
            | otherwise = xs 
----------------------------------------------------------------------------

-- Complexity is exponential in input size
choose ::  Int -> [b] -> [[b]]
choose k set
   | k == 0 || k > len  = [[]]
   |otherwise =  ((head set):) `fmap` (choose (k-1) (tail set)) ++ choose k (tail set)
   where len = length set
-----------------------------------------------------------------------------
-- Complexity is constant assuming xor is constant
greyCode :: Int -> Int
greyCode n = xor (n/2)  n
------------------------------------------------------------------------------
-- Complexity is linear in input size
cumSum :: (Integral b) => [b] -> b -> [b]
cumSum [] _ = []
cumSum sq sumSoFar 
    | tail sq == [] = sumSoFar + (head sq):[] 
    | otherwise = sumSoFar : (cumSum (tail sq) (sumSoFar + (head sq)))
-- Complexity is polynomial in input size
maximalSubsqSum xs
        | xs == [] = 0
        | otherwise = max (sum xs) (max (maxi (tail xs)) (maxi (init xs)))

