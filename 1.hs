
Types and Typeclasses

--Types are sets of values
--Typeclasses are sets of types

--------------------------------------------

Haskell                             Java

Typecalsses                         Interface
Type                                Class
Value                               Object

--------------------------------------------
--We can use :t to show the type

Prelude> :t True
True :: Bool

Prelude> :t 'a'
'a' :: Char

Prelude> :t "hkh"
"hkh" :: [Char]

Prelude> :t 3
3 :: Num a => a
-- Note: there is some types a in the typeclasses Num, 3 is of type a. a is called type variable. 

Prelude> :t (+)
(+) :: Num a => a -> a -> a

Prelude> :t zip
zip :: [a] -> [b] -> [(a, b)]

------------------------------------------------------------------

--write a function called type.hs

f ls = head ls + length ls

Prelude> :l type
[1 of 1] Compiling Main             ( type.hs, interpreted )
Ok, modules loaded: Main.
*Main> :t f
f :: [Int] -> Int

--then can define the type before function
f :: [Int] -> Int


--change the function
dividesEVenly :: Int -> Int -> Bool
dividesEvenly x y = (y \ x) * x == y

*Main> :l type
[1 of 1] Compiling Main             ( type.hs, interpreted )

type.hs:5:1: error:
    The type signature for ‘dividesEVenly’
      lacks an accompanying binding
Failed, modules loaded: none.

--as we can see, the (/) must be define in type Fractional
Prelude> :t (/)
(/) :: Fractional a => a -> a -> a

--change \ to 'div' ,funcion will be success.

                                       



