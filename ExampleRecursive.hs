
--The empty board
emptyBoard = ["...","...","..."]

--Returns the winner
winner :: [[Char]] -> Char
winner [[a,b,c],[d,e,f],[g,h,i]]
       | a == b && b == c && a /= '.' = a
       | d == e && e == f && d /= '.' = d
       | g == h && h == i && g /= '.' = g
       | a == d && d == g && a /= '.' = d
       | b == e && e == h && b /= '.' = b
       | c == f && i == f && c /= '.' = c
       | a == e && e == i && a /= '.' = a
       | c == e && e == g && c /= '.' = c
       | '.' `elem` [a,b,c,d,e,f,g,h,i] = '?'   -- Game not over
       | otherwise                      = '-'   --Tie
       
--Replaces the ith elem of a list
replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a :xs
replace i a (x:xs) = x:(replace(i-1) a xs)
