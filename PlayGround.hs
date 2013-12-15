import Data.List (find, delete, (\\))
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

data TrafficLight = Red | Yellow | Green  
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False 

instance Show TrafficLight where  
  show Red = "Red light"  
  show Yellow = "Yellow light"  
  show Green = "Green light"  

f :: (Monad m, Num b) => m b -> m b -> m b
f x y = do
  a <- x
  b <- y
  return (a*b)

--g :: [(a, b)]
g (x:_) = [curry x]

--filter' :: (x -> Bool) -> [x] -> [x]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

h [] = [[]]
h xs = concat [map (x:) (h (xs \\ [x])) | x <- xs]

oneOf :: Bool -> Bool -> Bool -> Bool
oneOf a b c = case (a,b,c) of
  (False, False, _) -> c
  (_, False, False) -> a
  (False, _, False) -> b
  (_, _, _) -> False

l = [(1, 2), (3, 4), (5, 6)]

ff :: Eq b => b -> Maybe a -> (b, a) -> Maybe a
ff key acc x@(k,v) =
  case acc of
    (Just x) -> (Just x)
    Nothing  -> check
  where check
          | key == k  = (Just v)
          | otherwise = Nothing

e k = do
  x <- k
  Nothing
  return False

mfmap f m = do
  v <- m
  return (f v)

q [] = []
q (x:xs) = x : q (filter (/=x) xs)

a x = do
  v <- x
  [v]


f' :: (Eq a) => [a] -> [a] -> [a]
f' = filter . flip elem

f'' s1 s2 = [s | s <- s2, elem s s1]