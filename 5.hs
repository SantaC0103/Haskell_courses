

Lambdas and folds
-----------------------------------------------------

--first, we have some functions

addAll :: (Num a) => [a] -> a
addAll [] = 0
addAll (x:xs) =x + addAll xs


multAll :: (Num a) => [a] -> a
multAll [] = 1
multAll (x:xs) = x*multAll xs

firstThat :: (a -> Bool) -> a -> [a] -> a
firstThat f = foldr(\x acc -> if f x then x else acc)

lastThat :: (a -> Bool) -> a -> [a] -> a 
lastThat f = foldl(\acc x -> if f x then x else acc)


argmax :: (Ord b) => ( a -> b) -> [a] -> a
argmax f [x] = x
argmax f (x:xs) = if f x > f (argmax f xs)
                  then x
                  else (argmax f xs)
                  
-----------------------------------------------------

-- text:

*Main>map(\x -> x+1) [1..5]
[2,3,4,5,6]
*Main> map(\s -> s+1) [2,4,5]
[3,5,6]

-- \ just lambdas,
-- lambdas allow us to create a function without given a name.Use of sometime we want to set a function just in one place.

*Main> map (\x -> 1 +(2*x)) [2,3,4]
[5,7,9]
*Main> map((1+).(2*))[2,3,4]   --a same function as lambdas, use .
[5,7,9]



*Main> firstThat (>0) 100 [-3,5,7,-7]
5
*Main> firstThat (>10) 100 [-3,5,7,-7]
100
*Main> argmax (>0) [2,4,5,8]
8
*Main> argmax(^2) [3,-5,4]
-5
*Main> argmax(+2) [3,-5,4]
4

-----------------------------------------------
--now try foldl and foldr in text:

foldl (+) 0 [3,2]
5
*Main> ((0+3)+2)
5
*Main> foldl (*) 1 [2,3,4]
24
*Main> ((1 * 2)*3)*4
24

*Main> foldl (-) 0 [1,2,3]
-6
*Main> ((0-1)-2)-3
-6

*Main> foldr (-) 0 [1,2,3]
2
*Main> 1-(2-(3-0))
2

------------------------------------------------------------

-- then use foldl to define functions more concisely

addAll :: (Num a) => [a] -> a
addAll = foldl (+) 0

multAll :: (Num a) => [a] -> a
multAll = fold (*) 1

argmax :: (Ord b) => (a->b) -> [a] -> a
argmax = foldl(\acc x -> if f x > f acc then x else acc)

