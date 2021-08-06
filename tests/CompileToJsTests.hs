{-# LANGUAGE QuasiQuotes #-}
module CompileToJsTests where

import Data.String.Interpolate ( i )
import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Compiler ( fullJS )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack)

build :: String -> IO String
build code = do
   (x, _, _) <- run (fullJS code) (Interp.env, classEnv) (env, [])
   return $ either show unpack x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

(--->) :: FilePath -> String -> Expectation
(--->) x y = do handle <- openFile x ReadMode
                contents <- hGetContents handle
                contents --> y

tests :: SpecWith ()
tests = do
  describe "Compile to JavaScript Tests" $ do

   it "value" $ "let main = 42" --> 
         "const main = ((fromInteger (numInt)) (42))"

   it "string value" $ "let main = \"Hello\"" -->
         "const main = \"Hello\""

   it "char value" $ "let main = 'h'" -->
         "const main = 'h'"

   it "applied function" $ [i|
      let f x = x + 1
      let main = f 5
   |] --> 
    [i|const f = function (numT2) {  return function (x) {  return (((__add (numT2)) (x)) (((fromInteger (numT2)) (1)))) } }
const main = ((f (numInt)) (((fromInteger (numInt)) (5))))|]

   it "factorial example" $ [i|
     let fac n = if n == 0 then 1 else n * fac (n - 1)
     let main = fac 5
   |] --> "const fac = function (eqT14) {  return function (numT14) {  return function (n) {  return function() { if ((((__eqeq (eqT14)) (n)) (((fromInteger (numT14)) (0))))) { return ((fromInteger (numT14)) (1)) } else { return (((__mul (numT14)) (n)) ((((fac (eqT14)) (numT14)) ((((__sub (numT14)) (n)) (((fromInteger (numT14)) (1)))))))) } }() } } }\nconst main = (((fac (eqInt)) (numInt)) (((fromInteger (numInt)) (5))))"

   it "factorial with lists" $ [i|
   let foldl f v xs =
      if (null xs) then v
      else foldl f (f v (head xs)) (tail xs)
      
   let range startIndex endIndex =
      let range' acc endIndex =
            if startIndex > endIndex then acc
               else range' (endIndex : acc) (endIndex - 1) in
            range' [] endIndex
         
   let fac n = foldl (*) 1 (range 1 n)    
         
   let main = fac 5     
   |] --> "const foldl = function (f) {  return function (v) {  return function (xs) {  return function() { if ((isEmpty (xs))) { return v } else { return (((foldl (f)) (((f (v)) ((head (xs)))))) ((tail (xs)))) } }() } } }\nconst range = function (numT24) {  return function (ordT24) {  return function (startIndex) {  return function (endIndex) { const rangeQuoted = function (acc) {  return function (endIndex) {  return function() { if ((((__gt (ordT24)) (startIndex)) (endIndex))) { return acc } else { return ((rangeQuoted (((__colon (endIndex)) (acc)))) ((((__sub (numT24)) (endIndex)) (((fromInteger (numT24)) (1)))))) } }() } }; return ((rangeQuoted ([])) (endIndex)) } } } }\nconst fac = function (numT13T19T6) {  return function (ordT13T19T6) {  return function (n) {  return (((foldl ((__mul (numT13T19T6)))) (((fromInteger (numT13T19T6)) (1)))) (((((range (numT13T19T6)) (ordT13T19T6)) (((fromInteger (numT13T19T6)) (1)))) (n)))) } } }\nconst main = (((fac (numInt)) (ordInt)) (((fromInteger (numInt)) (5))))"

   it "deconstucting tuples" $ [i|
      let f (x, y) = x + y
      let main = f (2, 3)
   |] --> "const f = function (numT3) {  return function ([x,y]) {  return (((__add (numT3)) (x)) (y)) } }\nconst main = ((f (numInt)) ([((fromInteger (numInt)) (2)),((fromInteger (numInt)) (3))]))"

   it "Parser Test 3" $ "tests/jnrs_lib/parser_example3.jnr" ---> "const bracket = function (monadm) {  return function (open) {  return function (p) {  return function (close) {  return (((bind (monadm)) (open)) (function (v1) {  return (((bind (monadm)) (p)) (function (x) {  return (((bind (monadm)) (close)) (function (v2) {  return ((pure (monadm)) (x)) })) })) })) } } } }\nconst item = (mkParser (function (inp) {  return function() { if ((isEmpty (inp))) { return [] } else { return ((__colon ([(head (inp)),(tail (inp))])) ([])) } }() }))\nconst zero = (mkParser (function (inp) {  return [] }))\nconst foldl = function (f) {  return function (v) {  return function (xs) {  return function() { if ((isEmpty (xs))) { return v } else { return (((foldl (f)) (((f (v)) ((head (xs)))))) ((tail (xs)))) } }() } } }\nconst or = function (p) {  return function (q) {  return (mkParser (function (inp) { const ret = ((runParser (p)) (inp)); return function() { if ((isEmpty (ret))) { return ((runParser (q)) (inp)) } else { return ret } }() })) } }\nconst negate = function (numT2) {  return function (x) {  return (((__sub (numT2)) (((fromInteger (numT2)) (0)))) (x)) } }\nconst sat = function (p) {  return (((bind (monadParser)) (item)) (function (x) {  return function() { if ((p (x))) { return ((pure (applicativeParser)) (x)) } else { return zero } }() })) }\nconst char = function (x) {  return (sat (function (y) {  return (((__eqeq (eqChar)) (x)) (y)) })) }\nconst digit = (sat (function (x) {  return ((__and ((((__lteq (ordChar)) ('0')) (x)))) ((((__lteq (ordChar)) (x)) ('9')))) }))\nconst foldl1 = function (f) {  return function (xs) {  return (((foldl (f)) ((head (xs)))) ((tail (xs)))) } }\nconst eval = function (xs) { const op = function (m) {  return function (n) {  return (((__add (numInt)) ((((__mul (numInt)) (((fromInteger (numInt)) (10)))) (m)))) (n)) } }; return ((foldl1 (op)) ((((bind (monadList)) (xs)) (function (x) {  return ((pure (applicativeList)) ((((__sub (numInt)) ((ord (x)))) ((ord ('0')))))) })))) }\nconst many = function (p) {  return ((or ((((bind (monadParser)) (p)) (function (x) {  return (((bind (monadParser)) ((many (p)))) (function (xs) {  return ((pure (applicativeParser)) (((__colon (x)) (xs)))) })) })))) (((pure (applicativeParser)) ([])))) }\nconst many1 = function (p) {  return (((bind (monadParser)) (p)) (function (x) {  return (((bind (monadParser)) ((many (p)))) (function (xs) {  return ((pure (applicativeParser)) (((__colon (x)) (xs)))) })) })) }\nconst nat = (((bind (monadParser)) ((many1 (digit)))) (((__dot ((pure (applicativeParser)))) (eval))))\nconst int = function () { const op = ((or ((((bind (monadParser)) ((char ('-')))) (function (v) {  return ((pure (applicativeParser)) ((negate (numInt)))) })))) (((pure (applicativeParser)) (function (x) {  return x })))); return (((bind (monadParser)) (op)) (function (f) {  return (((bind (monadParser)) (nat)) (function (n) {  return ((pure (applicativeParser)) ((f (n)))) })) })) }();\nconst sepby1 = function (p) {  return function (sep) {  return (((bind (monadParser)) (p)) (function (x) {  return (((bind (monadParser)) ((many ((((bind (monadParser)) (sep)) (function (v) {  return (((bind (monadParser)) (p)) (function (y) {  return ((pure (applicativeParser)) (y)) })) })))))) (function (xs) {  return ((pure (applicativeParser)) (((__colon (x)) (xs)))) })) })) } }\nconst ints = ((((bracket (monadParser)) ((char ('[')))) (((sepby1 (int)) ((char (',')))))) ((char (']'))))\nconst main = ((runParser (ints)) ((toCharList (\"[1,2,-5,-3,7]\"))))"