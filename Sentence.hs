module Sentence
where

import Data.List

compareSent :: String -> String -> Float
compareSent s1 s2 =
 compareWords (words s1) (words s2) (max (length (words s1)) (length (words s2)))

compareContext :: String -> String -> Float
compareContext s1 s2 = 
 compareWords (sort (words s1)) (sort (words s2)) (max (length (words s1)) (length (words s2)))
 
scoreSentence :: String -> String -> Float
scoreSentence s1 s2 = compareSent s1 s2 + compareContext s1 s2
 
compareWords :: [String] -> [String] -> Int -> Float
compareWords [] _ _ = 0
compareWords _ [] _ = 0
compareWords (s1:s1s) (s2:s2s) len
  | s1 == s2 = 1.0/(realToFrac len) + compareWords s1s s2s len
  | otherwise = compareWords s1s s2s len
  
bestSentence :: String -> [String] -> (String, Float)
bestSentence _ [] = ("Whoops!", 0)
bestSentence str [s] = (s, scoreSentence str s)
bestSentence str [s1,s2] 
 | (scoreSentence str s1) >= (scoreSentence str s2) = (s1, scoreSentence str s1)
 | otherwise = (s2, scoreSentence str s2)
bestSentence str (s:ss:sss)
 | scoreSentence str s >= scoreSentence str ss = bestSentence str (s:sss)
 | otherwise = bestSentence str (ss:sss)