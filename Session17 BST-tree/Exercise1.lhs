-----------------------------------------------------------------------------
-- |
-- Module      :  Examples
-- Copyright   :  (c) Ricardo Peña, March 2019               
-- License     :  LGPL
--
-- Maintainer  :  ricardo@sip.ucm.es
--
-- Liquid Haskell Binary Search Trees
-----------------------------------------------------------------------------

{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-} 

 
module Examples where
import Prelude hiding (head, max)
  
    
--
-- Some liquid functions and types
--
{-@ die :: {v:String | false} -> a  @-}
die msg = error msg
    
{-@ type Nat     = {v:Int | 0 <= v} @-}
{-@ type Pos     = {v:Int | 0 <  v} @-}
{-@ type NonZero = {v:Int | 0 /= v} @-}

-- This is the Haskell definition

data IncList a =
    Emp
  | (:<) { hd :: a, tl :: IncList a }

infixr 9 :<

-- This is the Liquid Haskell definition. Both are needed

{-@ data IncList a = Emp
                   | (:<) { hd::a, tl::IncList {v:a | hd <= v}} @-}


-- This is the Haskell definition

data BST a = Leaf 
           | Node { root  :: a
                  , left  :: BST a
                  , right :: BST a }

-- This is the Liquid Haskell definition. Both are needed
-- Note the order of the arguments in constructor Node

{-@ data BST a    = Leaf 
                  | Node { root  :: a
                         , left  :: BSTL a root
                         , right :: BSTR a root } @-}

{-@ type BSTL a X = BST {v:a | v < X}  @-}
{-@ type BSTR a X = BST {v:a | X < v}  @-}

goodBST =  Node 66
             (Node 4
                 (Node 1 Leaf Leaf)
                 (Node 61 Leaf Leaf))  -- Now is Ok for tree
             (Node 99
                 (Node 77 Leaf Leaf)
                 Leaf)

mem                 :: (Ord a) => a -> BST a -> Bool
mem _ Leaf          = False
mem k (Node k' l r)
    | k == k'         = True
    | k <  k'         = mem k l
    | otherwise       = mem k r


one   :: a -> BST a 
one x = Node x Leaf Leaf
  
-- LH proves here that the result is a BST
-- Try writing k' > k in line 87

add                  :: (Ord a) => a -> BST a -> BST a
add k' Leaf          = one k'
add k' t@(Node k l r)
      | k' < k       = Node k (add k' l) r
      | k  < k'      = Node k l (add k' r)
      | otherwise    = t

-- This says that the first element is smaller than all those in the second one

{-@ data MinPair a = MP { mElt :: a, rest :: BSTR a mElt} @-}
data MinPair a = MP { mElt :: a, rest :: BST a }


{-@ delMin :: NotLeaf a -> MinPair a @-}
delMin :: (Ord a) => BST a -> MinPair a
delMin (Node k Leaf r) = MP k r
delMin (Node k l r)    = MP k' (Node k l' r)
  where
    MP k' l'           = delMin l
delMin Leaf            = die "Don't say I didn't warn ya!" 
--needs an additional restriction
--add a notLeaf function to prevents BST a from being a Leaf


-- Exercise 1: try to avoid the above LH error

{-@ type NotLeaf a = {v:BST a | notLeaf v} @-}
{-@ measure notLeaf @-}
notLeaf :: BST a -> Bool
notLeaf Leaf = False
notLeaf (Node _ _ _) = True

-- Use of delMin to specify and program a delete function
-- The result must be a BST. Try writing x > k below instead of x < k

{-@ delete  :: (Ord a) => a -> BST a -> BST a @-}
delete   :: (Ord a) => a -> BST a -> BST a
delete x Leaf = Leaf
delete x t@(Node k l r)
      | x < k      = Node k (delete x l) r
      | k < x      = Node k l (delete x r)
--     | nonEmpty r = let MP m r' = delMin r in Node m l r' 
      | otherwise  = l


--nonEmpty Leaf           = False
--nonEmpty t@(Node _ _ _) = True
    



-- This proves that the traversal inorder of a BST is a sorted list

bstSort   :: (Ord a) => [a] -> IncList a
bstSort   = toIncList . toBST

toBST     :: (Ord a) => [a] -> BST a
toBST     = foldr add Leaf

-- Haskell prelude definition of foldr:
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z []     = z
-- foldr f z (x:xs) = f x (foldr f z xs)

-- Definition of toIncList

toIncList :: BST a -> IncList a
toIncList (Node x l r) = join x (toIncList l) (toIncList r)
toIncList Leaf         = Emp

--{-@ join :: a -> IncList a -> IncList a -> IncList a @-}

{-@ join :: x:a -> IncList ({v:a | v <= x}) -> IncList ({v:a | x <= v}) 
                -> IncList a @-}

join z Emp       ys = z :< ys 
join z (x :< xs) ys = x :< join z xs ys 
 

	
	
