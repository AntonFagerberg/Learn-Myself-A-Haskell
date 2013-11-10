module Quicksort (quicksort) where

quicksort :: Ord x => [x] -> [x]
quicksort (x:xs) = quicksort lte ++ [x] ++ quicksort gt
    where lte = filter (<= x) xs
          gt = filter (> x) xs
quicksort x = x