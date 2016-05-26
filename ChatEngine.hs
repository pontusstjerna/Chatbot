module ChatEngine
where

import AATree
import Data.Char
import Data.Maybe

word1 = Words "Hello" [(5,"there"), (4,"you"),(3,"!")]
word2 = Words "Hi" [(1, "there")]
word3 = Word "Hi"

words1 = addSent "Hello there you beautiful flower!" emptyTree

data WordSet a = Word a | Words a [(Integer, String)]
    deriving (Eq, Show, Ord)
    
succ :: WordSet String -> String
succ (Word _) = ""
succ (Words _ [(_, str)]) = str
succ (Words _ ((_,str):as)) = str

word :: WordSet String -> String
word (Word s) = s
word (Words s _) = s    

addSent :: String -> AATree (WordSet String) -> AATree (WordSet String)
addSent "" tree = tree
addSent (s:strs) tree = addWords (words ((toLower s):strs)) tree

addWords :: [String] -> AATree (WordSet String) -> AATree (WordSet String)
addWords [] tree = tree
addWords [str] tree | getWord' str tree == Nothing = insert (Word str) tree
                    | otherwise = tree
addWords (str:succ:strs) tree 
 | getWord' str tree == Nothing = insert (addSucc succ (Word str)) (addWords (succ:strs) tree)
 | otherwise = addWords (succ:strs) $update tree (fromJust (getWord' str tree)) succ addSucc

getWord' :: String -> AATree (WordSet String) -> Maybe (WordSet String)
getWord' s tree = getWord ((toLower (head s)):(tail s)) word tree

addSucc :: String -> WordSet String -> WordSet String
addSucc str (Word s) = Words s [(1, str)]
addSucc str (Words s succs) = (Words s (insertSucc str succs))
                                          
insertSucc :: String -> [(Integer, String)] -> [(Integer, String)]
insertSucc s [] = [(1, s)]
insertSucc s [(w, succ)] 
 | s == succ = [(w+1, succ)]
 | otherwise = [(w, succ), (1, s)]
insertSucc s ((w,succ):succs) 
 | s == succ = (w+1, succ):succs
 | otherwise = sortSuccs $ (w,succ):(insertSucc s succs)  
 
 
 
 
 
 

                              
sortSuccs :: [(Integer, String)] -> [(Integer, String)]
sortSuccs [] = []
sortSuccs [x] = [x]
sortSuccs xs = merge (sortSuccs ys) (sortSuccs zs)
 where
  n = div (length xs) 2
  ys = take n xs
  zs = drop n xs

merge :: [(Integer, String)] -> [(Integer, String)] -> [(Integer, String)]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | fst x > fst y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys