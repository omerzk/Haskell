gcd :: (Integral a) => a -> a -> a

gcd m n 
	| n == 0 = m
	| otherwise = gcd n (m 'mod' n)


insertIn ::(Integral a) => a -> b -> [b] -> [c] 

insertIn i e xs
  | i == 0 = e:xs 
  | i > length(xs) = "you'r an idiot" 
  | otherwise = head xs : (insertIn (i-1) e (tail xs))