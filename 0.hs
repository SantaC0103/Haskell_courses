Function(let, function,if..):


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

------------------------------------------------------------------------------

Note: let need to define the type system

*Main> let x=3.0
*Main> let y=4.0
*Main> hypotenuse x y
5.0
*Main> let x=3
*Main> let y=4
error

-------------------------------------------------------------------------------
Lists:

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





