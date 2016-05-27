module MergeSort
where

                           
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