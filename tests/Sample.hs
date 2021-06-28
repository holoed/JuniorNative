module Sample where

import Prelude hiding (foldl, foldr, (++), map, filter, reverse)

foldl:: (a -> b -> a) -> a -> [b] -> a
foldl f v xs =
  if null xs then v
  else foldl f (f v (head xs)) (tail xs)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v xs =
  if null xs then v
  else f (head xs) (foldr f v (tail xs))

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

join:: [[a]] -> [a]
join = foldl (++) []

map:: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

bind:: (a -> [b]) -> [a] -> [b]
bind f xs = join (map f xs)

sum:: Num a => [a] -> a
sum = foldl (+) 0

product:: Num a => [a] -> a
product = foldl (*) 1

filter:: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x : xs else xs) []

singleton:: a -> [a]
singleton x = [x]

quicksort:: Ord a => (b -> a) -> [b] -> [b]
quicksort f xs =
    if null xs then xs else
    let pivot = head xs in
    let rest = tail xs in
    let lessThan = filter (\x -> f x < f pivot) rest in
    let greaterThan = filter (\x -> f x > f pivot) rest in
    quicksort f lessThan ++ singleton pivot ++ quicksort f greaterThan

reverse:: [a] -> [a]
reverse = foldl (\xs x -> x : xs) []

range:: (Num a, Ord a) => (a -> b) -> a -> a -> [b]
range f startIndex endIndex =
 let range' acc endIndex =
      if startIndex > endIndex then acc
      else range' (f endIndex : acc) (endIndex - 1) in
      range' [] endIndex

partition:: (Eq a, Num a) => a -> [b] -> ([b], [b])
partition n xs =
  let partition' n acc xs =
       if n == 0 || null xs then (reverse acc, xs)
       else partition' (n - 1) (head xs : acc) (tail xs) in
  partition' n [] xs

split:: (Eq a, Num a) => a -> [b] -> [[b]]
split n xs =
  let split' acc xs =
        if null xs then acc
        else
        let pair = partition n xs in
            split' (fst pair : acc) (snd pair) in
        reverse (split' [] xs)

posToCoord:: Fractional a => a -> a -> (a, a)
posToCoord sz x =
  let row = x / sz in
  let col = x - sz * row in
  (row, col)

norm:: Num a => (a, a) -> a
norm (re, im) = re * re + im * im

cadd:: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
cadd (re1, im1) (re2, im2) = (re1 + re2, im1 + im2)

cmul:: Num a => (a, a) -> (a, a) -> (a, a)
cmul (re1, im1) (re2, im2) = (re1 * re2 - im1 * im2, re1 * im2 + im1 * re2)

mPoint :: (Ord b, Num a, Fractional b, Eq a) => a -> (b, b) -> (b, b) -> a
mPoint i c z =
  if i == 65 || norm z > 4.0 then i else mPoint (i + 1) c (cadd (cmul z z) c)

toDouble :: Int -> Double 
toDouble = fromIntegral

mandelbrot:: Int -> (Int, Int) -> (Int, Int, Int)
mandelbrot s (x, y) =
  let x' = 4.0 * toDouble y / toDouble s - 2.5 in
  let y' = 4.0 * toDouble x / toDouble s - 2.0 in
  let i' = mPoint 0 (x', y') (0.0, 0.0) in
  let f i = 128 + truncate (128.0 * cos (toDouble i + 0.3)) in
  (f i', f (i' + 16), f (i' + 32))
    