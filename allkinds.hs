gcd' :: (Integral a) => a -> a -> a

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
run :: (Eq a) => [a] -> [a]

run sq 
    | tail sq == [] = [] 
    | head sq == (head (tail sq)) = run (tail sq) 
    | otherwise = tail sq

runLength sq 
    | sq == [] = [] 
    |otherwise = (head sq, (length sq) - (length r)) : runLength r 
    where r = run sq 
----------------------------------------------------------------------------
subSets :: (Eq a , Integral b) => b -> [a] -> [[a]]

subSets k set = 
    | k > len || k == 0 = []
    | k == len = set 
    | head set : subSets (k - 1) (tail set):[] ++ subSets k (tail set)
    where len = length set

main = do putStrLn (show (runLength ['a','a','a','b','b'])) 