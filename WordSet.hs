module WordSet
where

import AATree
import Data.Char
import Data.Maybe
import MergeSort

word1 = Words "Hello" [(5,"there"), (4,"you"),(3,"!")]
word2 = Words "Hi" [(1, "there")]
word3 = Word "Hi"

words1 = addSent "Hello there you beautiful flower" emptyTree

data WordSet a = Word a | Words a [(Integer, String)]
    deriving (Eq, Show, Ord, Read)
    
successor :: WordSet String -> String
successor (Word _) = ""
successor (Words _ [(_, str)]) = str
successor (Words _ ((_,str):as)) = str

word :: WordSet String -> String
word (Word s) = s
word (Words s _) = s    

addSent :: String -> AATree (WordSet String) -> AATree (WordSet String)
addSent "" tree = tree
addSent (s:strs) tree = addWords (words ((toLower s):strs)) tree

 --(addAnswer userInp answer $read score)
 
addAnswer :: String -> String -> Integer -> AATree (WordSet String) -> AATree (WordSet String)
addAnswer uInp ans score tree 
 | getWord' uInp tree == Nothing = insert (Words uInp [(score, ans)]) tree word
 | otherwise = updateX tree (fromJustWordSet (getWord' uInp tree)) ans addSuccs 5

addWords :: [String] -> AATree (WordSet String) -> AATree (WordSet String)
addWords [] tree = tree
addWords [str] tree | getWord' str tree == Nothing = insert (Word str) tree word
                    | otherwise = tree
addWords (str:succ:strs) tree 
 | getWord' str tree == Nothing = insert (addSucc succ (Word str)) (addWords (succ:strs) tree) word
 | otherwise = addWords (succ:strs) $update tree (fromJust (getWord' str tree)) succ addSucc
 
addSents :: [String] -> AATree (WordSet String)
addSents sents = addSents' sents emptyTree
 where
 addSents' :: [String] -> AATree (WordSet String) -> AATree (WordSet String)
 addSents' [] tree = emptyTree
 addSents' [sent] tree = addSent sent tree
 addSents' (sent:sents) tree = addSents' sents $ addSent sent tree

getWord' :: String -> AATree (WordSet String) -> Maybe (WordSet String)
getWord' "" _ = Nothing
getWord' s tree = getWord s word tree

fromJustWordSet :: Maybe (WordSet String) -> WordSet String
fromJustWordSet (Just w) = w
fromJustWordSet _        = error "Not found!"

addSuccs :: String -> Int -> WordSet String -> WordSet String -- ugly hack
addSuccs _ 0 conv = conv
addSuccs str 1 conv = addSucc str conv
addSuccs str x conv | x > 5 = addSuccs str 5 conv
                    | x < 0 = conv
                    | otherwise = addSuccs str (x-1) (addSucc str conv)

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
 
constructNaive :: Maybe (WordSet String) -> AATree (WordSet String) -> String
constructNaive Nothing _ = ""
constructNaive (Just str) tree = (word str) ++ " " ++ constructNaive (getWord' (successor str) tree) tree

capital :: String -> String
capital (s:ss) = (toUpper s):ss
 
 