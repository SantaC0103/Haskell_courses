
Function,Lists,Tuples
------------------------------

1. function(let, function,if..):


write a function call hypotenuse.hs
----------------------------------------------------------------------------
hypotenuse a b = sqrt (a^2+b^2)


identifyCamel humps = if humps ==1
                      then "dromedary"
                      else "Bactrian"
-----------------------------------------------------------------------------

shuting@shuting-virtual-machine:~$ vim hypotenuse.hs
shuting@shuting-virtual-machine:~$ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help

Prelude> :load hypotenuse
[1 of 1] Compiling Main             ( hypotenuse.hs, interpreted )
Ok, modules loaded: Main.
*Main> hypotenuse 2 4
4.47213595499958
*Main> :l hypotenuse
[1 of 1] Compiling Main             ( hypotenuse.hs, interpreted )
Ok, modules loaded: Main.
*Main> identifyCamel 1
"dromedary"
*Main> identifyCamel 2
"Bactrian"

--Note: let need to define the type system

*Main> let x=3.0
*Main> let y=4.0
*Main> hypotenuse x y
5.0
*Main> let x=3
*Main> let y=4
error

-------------------------------------------------------------------------------
2. Lists:

Prelude> let num=[1,2,3,4,29,136]
Prelude> head num
1
Prelude> tail num
[2,3,4,29,136]
Prelude> tail(tail num)
[3,4,29,136]
Prelude> 
Prelude> tail(tail num)
[3,4,29,136]
Prelude> tail(tail(tail(tail(tail(tail num)))))
[]

--create a list
Prelude> 5:[]
[5]
Prelude> 1:5:[]
[1,5]
--add a number at the head of num
Prelude> 99: tail num
[99,2,3,4,29,136]


--some keyword:
-- Constant time: head, tail, null...
-- Liner time: length,reverse, !!, last, init, elem, ++, maximum, minimum, take, drop ...

--example:
Prelude> length num
6
Prelude> reverse num
[136,29,4,3,2,1]
-- !! show a4(start with a0) in the list num
Prelude> num !! 4
29
Prelude> last num
136
-- init show the list expct the last one
Prelude> init num
[1,2,3,4,29]
Prelude> null []
True
Prelude> null num
False
-- elem can check 15 in num or not
Prelude> elem 15 num
False
Prelude> elem 3 num
True
Prelude> [1,2,3]++[4,5,6]
[1,2,3,4,5,6]
Prelude> ['s','d','f']
"sdf"
Prelude> "adam"<"ant"
True
Prelude> sum num
175
Prelude> product num
94656
Prelude> sum [1..100]
5050
Prelude> ['a'..'f']
"abcdef"
Prelude> [2,4..100]
[2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100]
Prelude> [2,4,5..,100]

<interactive>:37:7: error: parse error on input ‘..’


--Note: just two number before.. when we want to have a list from 2 to 100

-- take
Prelude> take 3 [1..]
[1,2,3]
Prelude> take 9 [10..]
[10,11,12,13,14,15,16,17,18]

-- more example:

Prelude> [2^n | n <- [1..10]]
[2,4,8,16,32,64,128,256,512,1024]
Prelude> [2^n | n <- [1..10], 2^n >= 10, 2^n < 100]
[16,32,64]


Prelude> [x | x <- "outdfh", not (elem x "dfh")]
"out"
Prelude> [ x |x <- "outdfh", not(x`elem`"dfh")]
"out"


Prelude> [[ x | x <- word, not(x `elem` "bo")] | word <- ["bell","book","candle"]]
["ell","k","candle"]


Prelude> [[x*y | y <- [1..5]] | x <- [1..5]]
[[1,2,3,4,5],[2,4,6,8,10],[3,6,9,12,15],[4,8,12,16,20],[5,10,15,20,25]]
--each elem in y * each elem in x

---------------------------------------------------------------------------------

3. Tuples:
--tuples like a struct in C and a object in java


--We can see two diffrient lists equle or not, but can not see in tuples

Prelude> [1,2] == [1,2,3]
False
Prelude> (1,2) == (1,2,3)

<interactive>:8:10: error:
    • Couldn't match expected type ‘(Integer, Integer)’
                  with actual type ‘(Integer, Integer, Integer)’
    • In the second argument of ‘(==)’, namely ‘(1, 2, 3)’
      In the expression: (1, 2) == (1, 2, 3)
      In an equation for ‘it’: it = (1, 2) == (1, 2, 3)
      
--elem in lists must be have a same type, but tuples can have diffrient type elem

Prelude> [1,"yu"]

<interactive>:9:2: error:
    • No instance for (Num [Char]) arising from the literal ‘1’
    • In the expression: 1
      In the expression: [1, "yu"]
      In an equation for ‘it’: it = [1, "yu"]
Prelude> (1,"yu")
(1,"yu")


Prelude> fst("sd","ff")
"sd"

Prelude> snd("fw","er")
"er"

--zip turns two lists into a list of pairs

Prelude> zip["chen","li","huang"]["shu","ming","xue"]
[("chen","shu"),("li","ming"),("huang","xue")]


Prelude> let num = [1..4]
Prelude> let words = ["one",two","three","four"]
Prelude> let pairs = zip num words
Prelude> pairs
[(1,"one"),(2,"two"),(3,"three"),(4,"four")]




