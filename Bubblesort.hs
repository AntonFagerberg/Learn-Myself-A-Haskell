module Bubblesort (bubblesort) where
  
bubblesortIteration :: (Ord a) => [a] -> [a]
bubblesortIteration [] = []
bubblesortIteration [x] = [x]
bubblesortIteration (x1:x2:xs)
  | x1 <= x2 = x1 : bubblesortIteration(x2 : xs)
  | otherwise = x2 : bubblesortIteration(x1 : xs)


bubblesort :: (Ord a) => [a] -> [a]
bubblesort x
  | x == bx = x
  | otherwise = bubblesort bx
  where bx = bubblesortIteration x     