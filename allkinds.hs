gcd :: (Integral a) => a -> a -> a

gcd m n 
	| n == 0 = m
	| otherwise = gcd n (m 'mod' n)

--------------------------------------------------------------------------
insertIn ::(Integral a) => a -> b -> [b] -> [c] 

insertIn i e xs
  | i == 0 = e:xs 
  | i > length(xs) = "you'r an idiot" 
  | otherwise = head xs : (insertIn (i-1) e (tail xs))
---------------------------------------------------------------------------
lastNitems :: Int -> [a] -> [a]

lastNitems n xs
    | n > length xs = []
    | n == length xs = xs
    | otherwise = lastNitems (tail xs)
--.........................................
lastNitems' n xs = take n (reverse xs)
---------------------------------------------------------------------------
slice :: ->Int -> [a]-> ([a], [a])

slice k xs
    | k < 0 = ([],xs) 
    | k > length xs = (xs,[]) 
    | otherwise = ((take k xs),lastNitems ((length xs) - k))
--------------------------------------------------------------------------
runLength :: (Eq a) => [a] -> [(a, Int)]

run :: (Eq a) => [a] -> Int
 
runLength sq
    | sq == [] = []
    | head sq == (head (tail sq)) = (1 + (fst (runLength (tail sq))), head sq) : 
    | otherwise = 1 

