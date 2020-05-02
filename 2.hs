
FUnction syntax
----------------------------------------------------

increasing :: (Ord a) => [a] -> Bool
increasing xs = if xs =[]
             then if tail xs ==[]
                  then True
                  else if head xs <= head(tail xs)
                        then increasing (tail xs)
                        else False 


--As the same mean in Hasekll.
--This function means if list is null or one elem,must be True,
--and x is the first elem, y is the second, ys is a rest of y, if x<=y,then this list must be increasing.

increasing :: (Ord a) => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:ys) = x <= y && increasing(y:ys)

--Then,here is the same function in dafny
 predicate sorted(l:List<int>)
{
    match l 
       case Nil         => true
       case Cons(x, xs) => 
            match xs 
               case Nil         => true 
               case Cons(y, ys) => x <= y && sorted(xs)      
               
--text:

*Main> increasing [5]
True
*Main> increasing [4,2,1]
False
*Main> increasing [1,2,3]
True

--here is no increasing, return False.
*Main> increasing [1,3,2]
False


------------------------------------------------------

--This function means the list at least two elems then says any other list is increasing.
-- "_" matches everthing.

increasing :: (Ord a) => [a] -> Bool
increasing (x:y:ys) = x <= y && increasing(y:ys)
increasing _ =True

------------------------------------------------------


noVowels :: [Char] -> [Char]
noVowels word = if word ==""
                then ""
                else if head word `elem` "aeiouAEIOU"
                     then noVowels (tail word)
                     else (head word) : noVowels(tail word)


--text:
*Main> noVowels "Why you always do that."
"Why y lwys d tht."

--We don't need to use 'head' and 'tail'.

noVowels :: [Char] -> [Char]
noVowels "" = ""
noVowels (x:xs) = if x `elem` "aeiouAEIOU"
                  then noVowels xs
                  else x : noVowels xs


--and also, can use guard instead of if..else
--same result 

noVowels :: [Char] -> [Char]
noVowels "" = ""
noVowels (x:xs) 
           | x `elem` "aeiouAEIOU" = noVowels xs
           | otherwise             = x : noVowels xs

---------------------------------------------------------------


watch :: Int -> [Char]
watch n = if n==7
          then "Now,7 o'clcok and wakeup."
          else show n ++" o'clock and nothing"

*Main> watch 8
"8 o'clock and nothing"
*Main> watch 7
"Now,7 o'clcok and wakeup"

--matching witchout if..else!

watch :: Int -> [Char]
watch 7 = "7 o'clock and wakeup"
watch n = show n++ " o'clock and nothing"

--then, use where exp

watch :: Int -> [Char]
watch n = show n++ " o'clock and" ++ message n
          where message 7 = "wakeup"
                message _ = "nothing"


--aother, use case statement

watch :: Int -> [Char]
watch n = show n++ " o'clock and" ++ case n of 7 -> "wakeup"
                                               _ -> "nothing"
-------------------------------------------------------------------

gravity :: (Fractional a) => a -> a
gravity r = 6.674e-11 * 5.977e24 / (r^2)

*Main> gravity 6371000
9.827753342287505
*Main> gravity 0
Infinity

--then, we can using a let exp to give those constants names.

gravity :: (Fractional a) => a -> a
gravity r = let g = 6.674e-11 
                earthMass = 5.977e24 
              in g*earthMass / (r^2)

----------------------------------------------------------------------

--Note:

 pattern = result

--(guard)
 pattern
      | exp = result
      |..
      | otherwise = result

--(where)
 result where pattern = result

--(let in)
 let pattern = result
 ..
 in result
 
--(case)
 case exp of pattern -> result
 
--------------------------------------------------------------------

--tip:
--Haskel case about the speaes. 
 
 
