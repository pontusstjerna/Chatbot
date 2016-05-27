{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  isEmpty,       -- AATree a -> Bool
  leftSub,       -- AATree a -> AATree a
  rightSub,      -- AATree a -> AATree a
  rootVal,       -- AATree a -> a
  level,         -- AATree a -> Int
  get,           -- Ord a => a -> AATree a -> Maybe a
  getWord,       -- My own version
  update,        -- Updates an element with a given function
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  maxheight,     -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty |
                Node a Int (AATree a) (AATree a)
  deriving (Eq, Show, Read)

-- ALL FUNCTIONS WITHOUT COMMENTS HAVE O(1) COMPLEXITY

emptyTree :: AATree a
emptyTree = Empty

isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _     = False

leftSub :: AATree a -> AATree a
leftSub Empty = Empty
leftSub (Node _ _ left _) = left

rightSub :: AATree a -> AATree a
rightSub Empty = Empty
rightSub (Node _ _ _ right) = right

rootVal :: AATree a -> a
rootVal Empty = error "Tree is empty!"
rootVal (Node a _ _ _ ) = a
                     
level :: AATree a -> Int
level Empty = 0
level (Node _ lvl _ _) = lvl 

-- Complexity O(log n)
get :: Ord a => a -> AATree a -> (a -> String) -> Maybe a
get _ Empty _ = Nothing
get a (Node b _ left right) func
    | func a > func b = get a right func
    | func a < func b = get a left func
    | otherwise = Just b
    
-- My own special getter
getWord :: Ord a => String -> (a -> String) -> AATree a -> Maybe a
getWord _ _ Empty = Nothing
getWord a word (Node b _ left right)
    | a > word b = getWord a word right
    | a < word b = getWord a word left
    | otherwise = Just b
    
update :: Ord a => AATree a -> a -> b -> (b -> a -> a) -> AATree a
update Empty _ _ _ = Empty
update (Node a lvl t1 t2) ele arg func
 | ele == a = (Node (func arg a) lvl t1 t2)
 | otherwise = (Node a lvl (update t1 ele arg func)(update t2 ele arg func))

-- Insert places the node in its right place
-- and uses "fix" to fix the invariant
-- Complexity O(log n)
insert :: Ord a => a -> AATree a -> (a -> String) -> AATree a
insert a Empty _ = Node a 1 Empty Empty
insert a (Node b lvl t1 t2) func
       | func a <= func b = fix (Node b lvl (insert a t1 func) t2)
       | func a > func b = fix $ Node b lvl t1 (insert a t2 func)

-- If it's a leaf or empty, just leave it because the invariant is already true
-- First we check if there are three nodes at the root with the same level, if so, increase level of root
-- Then we check if the left child "is bad" (it has higher level than the right child) and if so, skew.
-- The left child is also bad if its level is the same as its parent, if so, also skew
-- If right grandchild is bad (it has the same level as its grandparent), split the tree
-- If it is not bad, just leave it
 where 
 fix :: AATree a -> AATree a
 fix Empty = Empty
 fix t@(Node _ _ Empty Empty) = t
 fix t@(Node v l tr1 tr2) 
  | level tr1 == level tr2 && level tr1 == l = Node v (l+1) tr1 tr2
  | level tr1 > level tr2 = skew t
  | level t == level tr1 = skew t -- If left child is bad
  | level t == level (rightSub tr2) = split t
  | otherwise = t      

insert _ _ _ = error "should never happen, but makes compiler happy.."

-- Complexity O(n)
inorder :: AATree a -> [a]
inorder t = inorder' t []
 where
  inorder' :: AATree a -> [a] -> [a]
  inorder' Empty        acc = acc
  inorder' (Node v _ l r) acc = inorder' l (v : inorder' r acc)

-- Complexity O(n)
size :: AATree a -> Int
size Empty = 0
size (Node _ _ Empty Empty) = 1
size (Node _ _ t1 t2) = 1 + size t1 + size t2

{-

size(n/2) + size(n/2)
,
-}

-- Complexity O(n)
maxheight :: AATree a -> Int
maxheight Empty = 0
maxheight (Node _ _ Empty Empty) = 1
maxheight (Node _ _ t1 t2) = 1 + (max (maxheight t1) (maxheight t2))

-- Complexity O(1)
split :: AATree a -> AATree a
split Empty = Empty
split (Node a lvl t1 t2) = Node (rootVal t2) (lvl+1) (Node a lvl t1 (leftSub t2)) (rightSub t2)

-- Complexity O(1)
skew :: AATree a -> AATree a
skew Empty = Empty
skew (Node a lvl t1 t2) = Node (rootVal t1) lvl (leftSub t1) (Node a lvl (rightSub t1) t2)

 
--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"
--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

-- Complexity O(n)
checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkNode (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- O(n)
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (a:as) = (a <= head as) && isSorted as

checkNode :: Ord a => AATree a -> Bool
checkNode node = leftChildOK node && rightChildOK node && rightGrandChildOK node

leftChildOK :: Ord a => AATree a -> Bool
leftChildOK (Node _ lvl l _) = lvl - 1 == level l
leftChildOK _                = True

rightChildOK :: Ord a => AATree a -> Bool
rightChildOK (Node _ lvl _ r) = lvl == level r || lvl - 1 == level r
rightChildOK _                = True

rightGrandChildOK :: Ord a => AATree a -> Bool
rightGrandChildOK (Node _ lvl _ r) = lvl - 1 == level (rightSub r) || 
                                     lvl - 2 == level (rightSub r)
rightGrandChildOK _ = True

--------------------------------------------------------------------------------
