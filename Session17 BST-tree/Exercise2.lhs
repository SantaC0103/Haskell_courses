 -- Exercise 2:  Define a Skew heap in Liquid Haskell and implemetnt
 -- and verify its operation join, joining two skew heaps 
 -- into one (see the dafny file to inspire you)
 
module SkewHeap where


import Data.List (foldl', unfoldr)
import Prelude hiding (minimum, maximum, null)


{-@ type AtLeast a X = {v:a | X <= v} @-}
{-@ data Skew a = Leaf 
                |Node {root  :: a
                      ,left  :: Skew (AtLeast a root)
                      ,right :: Skew (AtLeast a root) } @-}
                      
data Skew a = Leaf 
            | Node {root  :: a 
                   ,left  :: (Skew a) 
                   ,right :: (Skew a) }

instance Ord a => Eq (Skew a) where
  h1 == h2 = toSortedList h1 == toSortedList h2


{-@ empty :: Skew a @-}
empty :: Skew a
empty = Leaf

{-@ null :: Skew t -> Bool @-}
null Leaf         = True
null (Node _ _ _) = False

{-@ singleton :: a -> Skew a @-}
singleton :: a -> Skew a
singleton x = Node x Leaf Leaf

{-@ insert :: Ord a=> a -> Skew a -> Skew a @-}
insert :: Ord a => a -> Skew a -> Skew a
insert x t = merge (singleton x) t

{-@ fromList :: Ord a => [a] -> Skew a @-}
fromList :: Ord a => [a] -> Skew a
fromList [] = Leaf
fromList (x : xs) = insert x (fromList xs)

{-@ inorder :: a -> [a] -> [a] -> [a] @-}
inorder x [] l2 = x : l2
inorder x (h:t) l2 = h : inorder x t l2

{-@ destructNode :: Skew a -> (a, Skew a, Skew a) @-}
destructNode :: Skew a -> (a, Skew a, Skew a)
destructNode (Node x l r) = (x, l, r)

{-@ toList :: Skew a -> [a] @-}
toList :: Skew a -> [a]
toList Leaf = []
toList s =
   let (x', l', r') = destructNode s in
   inorder x' (toList l') (toList r')


{-@ minimum :: Skew a -> a @-}
minimum :: Skew a -> a
minimum (Node x _ _) = x

{-@ measure snd' @-}
snd' :: (a,b,c) -> b
snd' (_, x, _) = x

{-@ deleteMin :: Ord a => Skew a -> Skew a @-}
deleteMin :: Ord a => Skew a -> Skew a
deleteMin t@(Node _ _ _) = snd (deleteMin2 t)

{-@ deleteMin2 :: Ord a => Skew a ->(a, Skew a) @-}
deleteMin2 :: Ord a => Skew a ->(a, Skew a)
deleteMin2 (Node x l r)    = (x, merge l r)

{-@ merge :: Ord a => Skew a -> Skew a -> Skew a @-}
merge :: Ord a => Skew a -> Skew a -> Skew a
merge t1 Leaf = t1
merge Leaf t2 = t2
merge t1 t2
  | minimum t1 <= minimum t2 = join t1 t2
  | otherwise                = join t2 t1

{-@ join :: x:a -> Skew ({v:a | X <= v}) -> Skew a ->Skew a @-}
--{-@ join :: Ord a => Skew a -> Skew a -> Skew a @-}
join :: Ord a => Skew a -> Skew a -> Skew a
join (Node x l r) t = Node r x (join l t)



{-@ heapSort :: Ord a => [a] -> [a] @-}
heapSort :: Ord a => [a] -> [a]
heapSort = toSortedList . fromList

{-@ toSortedList :: Ord a => Skew a -> [a] @-}
toSortedList :: Ord a => Skew a -> [a]
toSortedList Leaf = []
toSortedList h@(Node _ _ _) =
    let (minElt, h') = deleteMin2 h in
    minElt : toSortedList h'

