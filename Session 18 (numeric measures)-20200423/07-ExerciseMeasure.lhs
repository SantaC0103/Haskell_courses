
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-termination" @-}

module NumericMeasures where
import Prelude  hiding  (map, zipWith, zip, take, drop, reverse)

{-@ type TRUE = {v:Bool | v} @-}

{-@ die :: {v:_ | false} -> a @-}
die msg = error msg
take, drop, take' :: Int -> [a] -> [a]
--txgo              :: Int -> Int -> Vector (Vector a) -> Vector (Vector a)
quickSort         :: (Ord a) => [a] -> [a]
size              :: [a] -> Int
flatten :: Int -> Int -> Vector (Vector a) -> Vector a

{-@ invariant {v:[a] | 0 <= size v} @-}

data Vector a = V { vDim  :: Int
                  , vElts :: [a]
                  }
              deriving (Eq)

data Matrix a = M { mRow  :: Int
                  , mCol  :: Int
                  , mElts :: Vector (Vector a)
                  }
              deriving (Eq)

dotProd       :: (Num a) => Vector a -> Vector a -> a
dotProd vx vy = sum (prod xs ys)
  where
    prod      = zipWith (\x y -> x * y)
    xs        = vElts vx
    ys        = vElts vy

matProd       :: (Num a) => Matrix a -> Matrix a -> Matrix a
matProd (M rx _ xs) (M _ cy ys)
                 = M rx cy elts
  where
    elts         = for xs $ \xi ->
                     for ys $ \yj ->
                       dotProd xi yj


{-@ measure size @-}
size []     = 0
size (_:rs) = 1 + size rs



{-@ measure notEmpty @-}
notEmpty       :: [a] -> Bool
notEmpty []    = False
notEmpty (_:_) = True



type List a = [a]

{-@ type ListN a N = {v:List a | size v = N} @-}
{-@ type ListX a X = ListN a {size X}        @-}


{-@ map      :: (a -> b) -> xs:List a -> ListX b xs @-}
--{-@ map      :: (a -> b) -> xs:List a -> {v:List b | size v == size xs} @-}
map _ []     = []
map f (x:xs) = f x : map f xs

{-@ prop_map :: List a -> TRUE @-}
prop_map xs = size ys == size xs
  where
    ys      = map id xs

{-@ reverse :: xs:List a -> ListX a xs @-}
reverse :: List a -> List a
reverse = go []
   where
     {-@ go :: ys:List a -> xs:List a -> ListN a {size xs + size ys} @-}
     go acc []     = acc
     go acc (x:xs) = go (x:acc) xs


--Exercise 1: Remove this LH error


{-@ zipWith :: (a -> b -> c) -> xs:List a
                             -> ListX b xs
                             -> ListX c xs
  @-}
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ [] []         = []
zipWith _ _  _          = die "no other cases"

{-@ zip :: as:[a] -> bs:[b] -> {v:[(a,b)] | Tinier v as bs} @-}
zip :: [a] -> [b] -> [(a, b)]
zip (a:as) (b:bs) = (a, b) : zip as bs
zip [] _          = []
zip _  []         = []

{-@ predicate Tinier X Y Z = Min (size X) (size Y) (size Z) @-}
{-@ predicate Min X Y Z = (if Y < Z then X = Y else X = Z)  @-}

--{-@ zipOrNull :: xs:[a] -> ys:[b] -> ListX [(a, b)] xs @-} This one still unsafe
zipOrNull [] _  = []
zipOrNull _ []  = []
zipOrNull xs ys = zipWith (,) xs ys

{-@ test1 :: {v: _ | size v = 2} @-}
test1 :: [(Int, Bool)]
test1     = zipOrNull [0, 1] [True, False]

{-@ test2 :: {v: _ | size v = 0} @-}
test2     = zipOrNull [] [True, False]

{-@ test3 :: {v: _ | size v = 0} @-}
test3     = zipOrNull ["cat", "dog"] []


-- Exercise 2: Fix the type of zipOrNull

{-@ take'     :: n:Nat -> ListGE a n -> ListN a n @-}
take' 0 _      = []
take' n (x:xs) = x : take' (n-1) xs
take' _ _      = die "won't  happen"

{-@ type ListGE a N = {v:List a | N <= size v} @-}

{-@ drop :: n:Nat -> v:ListGE a n -> { w:List a | size w = size v - n } @-}
drop 0 xs     = xs
drop n (_:xs) = drop (n-1) xs
drop _ _      = die "won't happen"

{-@ test4 :: ListN String 2 @-}
test4 = drop 1 ["cat", "dog", "mouse"]


-- Exercise 3: Fix the type of drop

{-@ predicate MinE X Y Z = if X < Y then Z == X else Z == Y @-}

{-@ take :: n: Nat -> v: List a -> { w: List a | MinE (size v) n (size w)} @-}
take 0 _       = []
take _ []      = []
take n (x:xs)  = x : take (n-1) xs

{-@ test5 :: [ListN String 2] @-}
test5 = [ take 2  ["cat", "dog", "mouse"]
        , take 20 ["cow", "goat"]        ]



-- Exercise 4: Fix the type of take

partition          :: (a -> Bool) -> [a] -> ([a], [a])
partition _ []     = ([], [])
partition f (x:xs)
  | f x            = (x:ys, zs)
  | otherwise      = (ys, x:zs)
  where
    (ys, zs)       = partition f xs

{-@ measure fst @-}
fst  (x, _) = x

{-@ measure snd @-}
snd (_, y) = y

{-@ partition :: _ -> xs:_ -> {v:_ | Sum2 v (size xs)} @-}
{-@ predicate Sum2 X N = size (fst X) + size (snd X) = N @-}

-- >> quickSort [1,4,3,2]
-- [1,2,3,4]

{-@ myconcat    :: xs:List a -> ys: List a -> {v:List a| size v == size xs + size ys}  @-}
myconcat [] ys     = ys
myconcat (x:xs) ys = x: (myconcat xs ys)

{-@ quickSort    :: (Ord a) => xs:List a -> ListX a xs @-}
quickSort []     = []
quickSort (x:xs) =  
  let ( nn, nm ) = partition (>= x) xs 
  in  myconcat nn (x:nm) 

{-@ test10 :: ListN String 2 @-}
test10 = quickSort test4


-- Exercise 5: complete the definition of quicksort


{-@ data Vector a = V { vDim  :: Nat
                      , vElts :: ListN a vDim }         @-}

{-@ vDim :: x:_ -> {v: Nat | v = vDim x} @-}

okVec  = V 2 [10, 20]       -- accepted by LH

badVec = V 2 [10, 20, 30]   -- rejected by LH

-- | Non Empty Vectors
{-@ type VectorNE a  = {v:Vector a | vDim v > 0} @-}

-- | Vectors of size N
{-@ type VectorN a N = {v:Vector a | vDim v = N} @-}

-- | Vectors of Size Equal to Another Vector X
{-@ type VectorX a X = VectorN a {vDim X}        @-}

{-@ vEmp :: VectorN a 0 @-}
vEmp = V 0 []

{-@ vCons :: a -> x:Vector a -> VectorN a {vDim x + 1} @-}
vCons x (V n xs) = V (n+1) (x:xs)

{-@ vHd :: VectorNE a -> a @-}
vHd (V _ (x:_))  = x
vHd _            = die "nope"

{-@ vTl          :: x:VectorNE a -> VectorN a {vDim x - 1} @-}
vTl (V n (_:xs)) = V (n-1) xs
vTl _            = die "nope"

{-@ for        :: x:Vector a -> (a -> b) -> VectorX b x @-}
for (V n xs) f = V n (map f xs)

{-@ vBin :: (a -> b -> c) -> x:Vector a
                          -> VectorX b x
                          -> VectorX c x
  @-}
vBin op (V n xs) (V _ ys) = V n (zipWith op xs ys)

{-@ dotProduct :: (Num a) => x:Vector a -> VectorX a x -> a @-}
dotProduct x y = sum $ vElts $ vBin (*) x y

{-@ vecFromList     :: w:[a] -> { v: Vector a | vDim v == size w} @-}
vecFromList     :: [a] -> Vector a
vecFromList xs  =  V (size xs) xs

test6  = dotProduct vx vy    -- should be accepted by LH
  where
    vx = vecFromList [1,2,3]
    vy = vecFromList [4,5,6]


-- Exercise 6: refine the type of vecFromList


{-@ flatten :: n:Nat
            -> m:Nat
            -> VectorN (VectorN a m) n
            -> VectorN a {m * n}
   @-}
flatten = 0 _ _ =  V 0 []
flatten = 
   let vRestFlattened = flatten (n-1) m (vecFromList xs)
   in n m (V _ (x:xs)) = vecFromList (concat1 (vElts x) (vElts vRestFlattened))


-- Exercise 7: write a suitable code for flatten
{-@ product   :: xs:Vector _
              -> ys:Vector _
              -> VectorN _ {vDim xs * vDim ys}
  @-}
product xs ys = flatten (vDim ys) (vDim xs) xys
  where
    xys       = for ys $ \y ->
                  for xs $ \x ->
                    x * y

{-@ data Matrix a =
      M { mRow  :: Pos
        , mCol  :: Pos
        , mElts :: VectorN (VectorN a mCol) mRow
        }
  @-}

{-@ type Pos = {v:Int | 0 < v} @-}

{-@ type MatrixN a R C   = {v:Matrix a | Dims v R C } @-}
{-@ predicate Dims M R C = mRow M = R && mCol M = C   @-}

{-@ ok23 :: MatrixN _ 2 3 @-}
ok23     = M 2 3 (V 2 [ V 3 [1, 2, 3]
                      , V 3 [4, 5, 6] ])

bad1 :: Matrix Int
bad1 = M 2 3 (V 2 [ V 3 [1, 2  3 ]
                  , V 3 [4, 5, 6]])

bad2 :: Matrix Int
bad2 = M 2 2 (V 2 [ V 2 [1, 2]
                  , V 2 [4, 5] ])

-- Exercise 8: complete this trivial exercise

{-@ measure insize @-}
{-@ insize      :: [[a]] -> Int @-}
insize          :: [[a]] -> Int
insize [] = 0
insize (x:xs) = size x

{-@ matFromList      :: xss:[[a]] -> Maybe (MatrixN a (size xss) (insize xss)) @-}
matFromList      :: [[a]] -> Maybe (Matrix a)
matFromList []   = Nothing
matFromList xss@(xs:_)
  | ok           = Just (M r c vs)
  | otherwise    = Nothing
  where
    r            = size xss
    c            = size xs
    ok           = undefined 
    vs           = undefined
--Still can not comlete  the code, and type also unsafe

{-@ mat23 :: Maybe (MatrixN Integer 3 2) @-}
mat23     = matFromList [ [1, 2]
                        , [3, 4] 
    					, [4, 5]]

-- Exercise 9: complete the code and refine the type of matFromList


{-@ matProduct :: (Num a) => x:Matrix a
                          -> y:{Matrix a  | mCol x = mRow y}
                          -> MatrixN a (mRow x) (mCol y)
  @-}
matProduct (M rx _ xs) my@(M _ cy _)
                 = M rx cy elts
  where
    elts         = for xs $ \xi ->
                     for ys' $ \yj ->
                       dotProduct xi yj
    M _ _ ys'    = transpose my

-- >>> ok32 == transpose ok23
-- True
ok32 = M 3 2 (V 3 [ V 2 [1, 4]
                  , V 2 [2, 5]
                  , V 2 [3, 6] ])

{-@ transpose :: m:Matrix a -> MatrixN a (mCol m) (mRow m) @-}
transpose (M r c rows) = M c r $ txgo c r rows

{-@ txgo      :: c:Nat -> r:Nat
              -> VectorN (VectorN a c) r
              -> VectorN (VectorN a r) c
  @-}
txgo c r rows = undefined


-- Exercise 10: complete the code of transpose

