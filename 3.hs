
Recursive

A recursive function is one that calls itself, a powerful technique for solving problems in terms of easier problems.

-Baes cases ( edge conditions)
-Recursive cases

------------------------------------------------------------

--some useful function for lists

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: (Num a) => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs
           
maximum' :: (Ord a) => [a] -> a
--maximum' [] = error "no maximum in empty list"
maximum' [x] = x
maximum' (x:xs)
           | x > mx     = x
           | otherwise  = mx
           where mx = maximum' xs
           
           

        
