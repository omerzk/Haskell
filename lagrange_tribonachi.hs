
isInt x = x == fromInteger (round x)

lagrange n = [round m|m<-[1..n], m `elem` [ 1, 3, 5, 7, 11, 15, 23 ] || isInt (4 `logBase` (m/2))||isInt(4 `logBase` (m/6))|| isInt(4 `logBase` (m/14))]

rec_comb n = if n == 0 then 1 else if n < 0 then 0 else rec_comb (n-1) + rec_comb (n-2) + rec_comb (n-3)

comb =  1 : 1 : 2 : zipWith (+) comb (zipWith (+) (tail comb) (tail (tail comb)))