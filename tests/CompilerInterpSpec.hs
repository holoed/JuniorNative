{-# LANGUAGE QuasiQuotes #-}
module CompilerInterpSpec where

import Data.String.Interpolate ( i )
import Test.Hspec (Spec, shouldBe, describe, it, Expectation, parallel)
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Compiler ( fullInterp )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack)

interpPrelude :: String
interpPrelude = [i|
val id :: a -> a
let id x = x

val fst :: (a, b) -> a
let fst (x, y) = x

val snd :: (a, b) -> b
let snd (x, y) = y

val (.) :: (b -> c) -> (a -> b) -> a -> c
let (.) f g = \\x -> f (g x)
|]


build :: String -> IO String
build code = do
   (x, _, _) <- run (fullInterp (interpPrelude <> "\r\n\r\n" <> code)) ("main", Interp.env, classEnv) (env, [])
   return $ either show unpack x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

(--->) :: FilePath -> String -> Expectation
(--->) x y = do handle <- openFile x ReadMode
                contents <- hGetContents handle
                contents --> y

spec :: Spec
spec = parallel $ do 
  describe "Compiler Tests" $ do

   it "value" $ do
     "let main = 42" --> "42"
     "let main = 'H'" --> "'H'"
     "let main = ' '" --> "' '"
     "let main = \"2 + 3\"" --> "\"2 + 3\""
     "let main = \"    \"" --> "\"    \"" 

   it "applied function" $ [i|
      let f x = x + 1
      let main = f 5
   |] --> "6"

   it "recursive function" $ [i|
      let fac n = if n == 0 then 1 else n * (fac (n - 1))
      let main = fac 5
   |] --> "120"

   it "recursive function 2" $ [i|
      let fib n = if n == 0 then 0 else 
                  if n == 1 then 1 else 
                  fib (n - 1) + fib (n - 2)
      let main = fib 10
   |] --> "55"

   it "depending on a top level value affected by monomorphic restriction" $ [i|
      let x = 42
      let main = x + 1
   |] --> "43"

   it "higher order recursive function" $ [i|
      let foldright f v xs =
      if (null xs) then v
      else f (head xs) (foldright f v (tail xs))
      
      let xs = 1:2:3:4:5:[]

      let main = foldright (*) 1 xs 
   |] --> "120"

   it "higher order recursive function 2" $ [i|
      let foldl f v xs =
          if (null xs) then v
          else foldl f (f v (head xs)) (tail xs)
      
      let xs = 1:2:3:4:5:[]

      let main = foldl (*) 1 xs 
   |] --> "120"

   it "Predicates Resolution 1" $ [i|
   let foldright f v xs =
      if (null xs) then v
      else f (head xs) (foldright f v (tail xs))

   let (++) xs ys = foldright (:) ys xs 

   let main = pure 42 ++ []
   |] --> "[42]"

   it "Predicates Resolution 2" $ [i|
   let foldl f v xs =
       if (null xs) then v
       else foldl f (f v (head xs)) (tail xs)

   let foldright f v xs =
       if (null xs) then v
       else f (head xs) (foldright f v (tail xs))

   let mapM f as = 
       let k a r = bind (f a) (\\x ->
                   bind r     (\\xs -> 
                   pure (x:xs))) in   
       foldright k (pure []) as
      
   let main = mapM (\\x -> x:[]) (1:2:3:4:[])
   |] --> "[[1,2,3,4]]"
 
   it "Ord instances" $ do 
      "let main = 2 > 5" --> "False"
      "let main = 3 < 4" --> "True"
      "let main = 4 >= 3" --> "True"
      "let main = 5 <= 2" --> "False"

   it "Eq instances" $ do 
      "let main = 2 == 5" --> "False"
      "let main = 3 == 3" --> "True"
      "let main = True == True" --> "True"
      "let main = True == False" --> "False"
      "let main = \"Hello\" == \"World\"" --> "False"
      "let main = \'h\' == \'x\'" --> "False"
      "let main = 2 /= 3" --> "True"

   it "Num instances" $ do 
      "let main = 2 + 5" --> "7"
      "let main = 3 - 3" --> "0"
      "let main = 6 * 3" --> "18"

   it "Integral instances" $ do
      "let main = mod 5 3" --> "2"

   it "Complex numbers" $ do 
      "let main = (2, 3) + (5, 6)" --> "(7,9)"
      "let main = (3, 9) - (2, 5)" --> "(1,4)"
      "let main = (7, 3) * (2, 5)" --> "(-1,41)"
      "let main = (2, 3) + 1" --> "(3,4)"
      "let main = 1 + (2, 3)" --> "(3,4)"
      "let main = (2, 3) - 1" --> "(1,2)"

   it "List syntax" $ do
      "let main = []" --> "[]"
      "let main = [1]" --> "[1]"
      "let main = [1,2,3]" --> "[1,2,3]"
      "let main = [1,2,3,4]" --> "[1,2,3,4]"
      "let main = [\'h\', \'e\']" --> "[\'h\',\'e\']"
      "let main = [1 + 2, 3 * 4]" --> "[3,12]"
      "let main = [[1,2],[3,5]]" --> "[[1,2],[3,5]]"

   it "Quicksort example" $
      [i|let foldright f v xs =
            if (null xs) then v
            else f (head xs) (foldright f v (tail xs))

         let (++) xs ys = foldright (:) ys xs
            
         let filter p = foldright (\\x xs -> if (p x) then x : xs else xs) []
                  
         let singleton x = x : []
                  
         let quicksort f xs =
               if (null xs) then xs else  
               let pivot = head xs in
               let rest = tail xs in
               let lessThan = filter (\\x -> f x < f pivot) rest in
               let greaterThan = filter (\\x -> f x >= f pivot) rest in
               (quicksort f lessThan) ++ (singleton pivot) ++ (quicksort f greaterThan)
               
         let main = quicksort (\\x -> x) (5:2:9:5:3:1:2:8:5:3:[]) 
       |] --> "[1,2,2,3,3,5,5,5,8,9]"

   it "Defaulting mixed Fractional and Num" $ 
       [i|let f x = x + 1.5
          let main = f 5       
          |] --> "6.5"

   it "Arithmetics Tests" $ do
      "let main = (1 + 1.5) / 3" --> "0.8333333333333334"
      "let main = 2 * (4.6 - 3.1)" --> "2.999999999999999"

   it "Lambda many tuples pattern" $ do
        [i|
            let f (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
            let main = f (2, 3) (5, 6)
        |] --> "(7,9)"

   it "deconstucting tuples in lets" $ [i|
      let add (z1, z2) = 
      let (a, b) = z1 in
      let (c, d) = z2 in
      (a + c, b + d)
      
      let main = add ((2, 3), (4, 5)) 
   |] --> "(6,8)"

   it "Nested let with variable capture" $ do
         [i|let f x = x + 1.5
            let g x =
               let y = f x in
               (x,y)
            let main = g 5|] --> "(5.0,6.5)"

   it "Predicates with predicates" $ do
      [i|let main = (2, 3) == (4, 7)|] --> "False"
      [i|let main = (2, 3) == (2, 3)|] --> "True"

   it "Logical operators" $ do
      "let main = True && True" --> "True"
      "let main = True && False" --> "False"
      "let main = False && True" --> "False"
      "let main = False && False" --> "False"
      "let main = True || True" --> "True"
      "let main = True || False" --> "True"
      "let main = False || True" --> "True"
      "let main = False || False" --> "False"

   it "from List to Map" $ do
      "let main = fromListToMap (('a', 2):[])" --> "{'a':2}"

   it "Fix value level example" $ do [i|
      let fix f x = f (fix f) x
      let fac f n = if n == 0 then 1 else n * f (n - 1)
      let facRec = fix fac
      let main = facRec 5|] --> "120"

   it "Floating operators" $ do
      [i| 
 let normal (re, im) = 
    re * re + im * im
 
 let complex_add (re1, im1) (re2, im2) = 
    (re1 + re2, im1 + im2)
 
 let complex_mul (re1, im1) (re2, im2) = 
    (re1 * re2 - im1 * im2, re1 * im2 + im1 * re2)
 
 let mid_point i c z =
  if (i == 65 || normal z > 4.0) then i 
  else mid_point (i + 1) c (complex_add (complex_mul z z) c)
   
 let foo s (x, y) =
  let x' = (4.0 * toDouble y / toDouble s) - 2.5 in
  let y' = (4.0 * toDouble x / toDouble s) - 2.0 in
  let i' = mid_point 0 (x', y') (0.0, 0.0) in
  let f i = 128 + truncate (128.0 * (cos (toDouble i + 0.3))) in 
  ((f i'), (f (i' + 16)), (f (i' + 32))) 
    
 let main = foo 2 (3, 4) |] --> "(162,130,89)"

   it "Instance construction - Function Equality of Tuples" $
      [i|
      let f x y = (x, y) == (x, y)
      let main = f 2 4
      |] --> "True"

   it "Instance construction - Function Equality of Nested Tuples" $
      [i|
      let f x y z = (x, (y, z)) == (x, (y, z))
      let main = f 2 4 5
      |] --> "True"

   it "Functor test" $
      [i|
      let xs = 1:2:3:4:[]
      let main = fmap (\\x -> x * x) xs 
      |] --> "[1,4,9,16]"

   it "Applicative operator test" $
      [i|
      let main = (\\x -> x + x):(\\x -> x * x):[] <*> 5:6:[]
      |] --> "[10,12,25,36]"

   it "Parser Test 1" $ "tests/jnrs_lib/parser_example1.jnr" ---> "[(['H','e'],['l','l','o'])]"

   it "Parser Test 2" $ "tests/jnrs_lib/parser_example2.jnr" ---> "[(['H','e','l','l','o'],[])]"

   it "Parser Test 3" $ "tests/jnrs_lib/parser_example3.jnr" ---> "[([1,2,-5,-3,7],[])]"

   it "Calculator Test" $ "tests/jnrs_lib/calculator.jnr" ---> "[(5,[])]"

   it "Calculator Test 2" $ "tests/jnrs_lib/calculator_with_spaces.jnr" ---> "[(14,[])]"

   it "Colors Test" $ "tests/jnrs_lib/color_conversion.jnr" ---> "([(255,0,0),(0,255,0),(0,0,255)],[(127,127,127),(127,0,0),(255,127,127)])"
   
   it "Plasma Test" $ "tests/jnrs_lib/plasma_example.jnr" ---> "[[(255,0,0),(255,1,0),(255,2,0),(255,3,0),(255,5,0)],[(255,1,0),(255,2,0),(255,3,0),(255,4,0),(255,5,0)],[(255,2,0),(255,3,0),(255,4,0),(255,5,0),(255,6,0)],[(255,3,0),(255,4,0),(255,5,0),(255,6,0),(255,7,0)],[(255,4,0),(255,4,0),(255,5,0),(255,6,0),(255,7,0)],[(255,5,0)]]"

   -- it "Duality of Sorts" $ "tests/jnrs_lib/duality_of_sorts.jnr" ---> "[0,1,3,5,6,9]"