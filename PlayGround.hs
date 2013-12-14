import Data.List (find, delete)
import System.Random
maxi :: Ord a => a -> a -> a
maxi x y
  | x >= y    = x
  | otherwise = y

sumsq :: (Enum a, Num a) => a -> a
sumsq n = foldr1 (+) . map (\x -> x*x) $ [0..n]

smallestFactor :: (Integral a) => a -> (Maybe a)
smallestFactor n = find (\x -> n `mod` x == 0) [2 .. n]

multiply :: (Num a) => [a] -> a
multiply = foldr1 (*)

substitute :: Eq c => c -> c -> [c] -> [c]
substitute x y = map (fun)
  where fun c
            | c == x = y
            | otherwise = c

duplicates :: Eq a => [a] -> Bool
duplicates []   = False
duplicates (x:xs) = case (elem x xs) of True -> True
                                        False -> duplicates xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | elem x xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

isPermutation :: Eq a => [a] -> [a] -> Maybe ()
isPermutation [] [] = Just ()
isPermutation (x:xs) y = do
  i <- find (==x) y
  isPermutation xs (delete i y)

data Set = IntSet [Int] | StringSet String deriving (Show)

createSet :: [Int] -> Set
createSet x = (IntSet x)

createSSet :: String -> Set
createSSet x = (StringSet x)

f1 x y = foldr (:) x y
f2 x y = foldr (const (f1 x)) [] y
f3 x y = foldr (const (f2 x)) [()] y

{-- snippet RandomState --}
type RandomState a = State StdGen a
{-- /snippet RandomState --}

{-- snippet getRandom --}
getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val
{-- /snippet getRandom --}

{-- snippet getRandomDo --}
getRandomDo :: Random a => RandomState a
getRandomDo = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val
{-- /snippet getRandomDo --}