module Sample where

import Prelude hiding (foldl, foldr, (++), map, filter, reverse)

foldl f v xs =
  if null xs then v
  else foldl f (f v (head xs)) (tail xs)

foldr f v xs =
  if null xs then v
  else f (head xs) (foldr f v (tail xs))

(++) xs ys = foldr (:) ys xs

join = foldl (++) []

map f = foldr (\x xs -> f x : xs) []

bind f xs = join (map f xs)

sum = foldl (+) 0

product = foldl (*) 1

filter p = foldr (\x xs -> if p x then x : xs else xs) []

singleton x = [x]

quicksort f xs =
    if null xs then xs else
    let pivot = head xs in
    let rest = tail xs in
    let lessThan = filter (\x -> f x < f pivot) rest in
    let greaterThan = filter (\x -> f x > f pivot) rest in
    quicksort f lessThan ++ singleton pivot ++ quicksort f greaterThan

reverse = foldl (\xs x -> x : xs) []

range f startIndex endIndex =
 let range' acc endIndex =
      if startIndex > endIndex then acc
      else range' (f endIndex : acc) (endIndex - 1) in
      range' [] endIndex

partition n xs =
  let partition' n acc xs =
       if n == 0 || null xs then (reverse acc, xs)
       else partition' (n - 1) (head xs : acc) (tail xs) in
       partition' n [] xs

split n xs =
  let split' acc xs =
        if null xs then acc
        else
        let pair = partition n xs in
            split' (fst pair : acc) (snd pair) in
        reverse (split' [] xs)

posToCoord sz x =
  let row = x / sz in
  let col = x - sz * row in
  (row, col)

norm (re, im) = re * re + im * im

cadd (re1, im1) (re2, im2) = (re1 + re2, im1 + im2)

cmul (re1, im1) (re2, im2) = (re1 * re2 - im1 * im2, re1 * im2 + im1 * re2)

mPoint i c z =
  if i == 65 || norm z > 4.0 then i else mPoint (i + 1) c (cadd (cmul z z) c)

toDouble :: Int -> Double 
toDouble = fromIntegral

mandelbrot s (x, y) =
  let x' = 4.0 * toDouble y / toDouble s - 2.5 in
  let y' = 4.0 * toDouble x / toDouble s - 2.0 in
  let i' = mPoint 0 (x', y') (0.0, 0.0) in
  let f i = 128 + truncate (128.0 * cos (toDouble i + 0.3)) in
  (f i', f (i' + 16), f (i' + 32))
    