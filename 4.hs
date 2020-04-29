
Hight order functions

------------------------------

--Some simple function

add1 :: Int -> Int
add1 x = x + 1

f :: (Int -> Int) -> Int
f x = 3

g :: Int -> (Int -> Int)
g x = add1

add1ToEach :: [Int] -> [Int]
add1ToEach [] = []
add1ToEach (x:xs) = add1 x : add1ToEach xs

--h :: int -> Int -> Int
h :: Int -> (Int -> Int)
h x y = x+y


--sum3 :: Int -> Int -> Int -> Int
sum3 :: Int -> (Int -> (Int -> Int))
sum3 a b c = a + b + c

from :: Int -> Int -> Int
from = flip(-)


--------------------------------------------

--text to show map
*Main> map add1[1,2,3,4]
[2,3,4,5]
*Main> map (max 2) [1,2,3,4]
[2,2,3,4]
*Main> map(10/)[1,2,3]
[10.0,5.0,3.3333333333333335]
*Main> map(/10)[1,2,3]
[0.1,0.2,0.3]

--map is built-in , but could be define by this
map :: (a -> b) -> [a] -> [b]
map _[] = []
map f (x:xs) = f x :map f xs

---------------------------------------------

*Main> h 4 5
9
*Main> (h 4) 5
9
*Main> h 6

<interactive>:9:1: error:
    • No instance for (Show (Int -> Int)) arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
*Main> let hmm = h 6
*Main> hmm 2
8


*Main> sum3 7 8 9
24
*Main> ((sum3 7) 8) 9
24

*Main> 5`from`8
3
*Main> from 5 8
3


--Higher order function have buit-in zipWith,filter,takeWhile...

*Main> zipWith (+) [1,2,3] [4,5,6]
[5,7,9]
*Main> zipWith (*) [1,2,3] [4,5,6]
[4,10,18]

*Main> filter(>5) [1..10]
[6,7,8,9,10]

*Main> takeWhile(<10)[1,3..]
[1,3,5,7,9]
