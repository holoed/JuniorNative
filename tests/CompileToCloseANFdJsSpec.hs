{-# LANGUAGE QuasiQuotes #-}
module CompileToCloseANFdJsSpec where

import Data.String.Interpolate ( i )
import Test.Hspec (Spec, shouldBe, describe, it, Expectation, parallel)
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Compiler ( fullJSClosedANF )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack)
import JavaScriptRunner (runJS)

build :: String -> IO String
build code = do
   let libPath = "src/javascript/baseClosedLib.js"
   (x, _, _) <- run (fullJSClosedANF code) (Interp.env, classEnv) (env, [])
   either (return . show) (runJS libPath . unpack) x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

(--->) :: FilePath -> String -> Expectation
(--->) x y = do handle <- openFile x ReadMode
                contents <- hGetContents handle
                contents --> y

spec :: Spec
spec = parallel $ do
  describe "Compile to Closed and ANF JavaScript Tests" $ do

   it "value" $ "let main = 42" --> "42"

   it "string value" $ "let main = \"Hello\"" --> "\"Hello\""

   it "char value" $ "let main = 'h'" --> "\"h\""

   it "applied function" $ [i|
      let f x = x + 1
      let main = f 5
   |] --> "6"

   it "factorial example" $ [i|
     let fac n = if n == 0 then 1 else n * fac (n - 1)
     let main = fac 5
   |] --> "120"

   it "Fix value level example" $ do [i|
      let fix f x = f (fix f) x
      let fac f n = if n == 0 then 1 else n * f (n - 1)
      let facRec = fix fac
      let main = facRec 5|] --> "120"

   it "factorial with lists" $ [i|
   let foldl f v xs =
      if (null xs) then v
      else foldl f (f v (head xs)) (tail xs)
      
   let fac n = foldl (*) 1 (range (\\x-> x) 1 n)    
         
   let main = fac 5     
   |] --> "120"    

   it "Arithmetics Tests" $ do
      "let main = (1 + 1.5) / 3" --> "0.8333333333333334"
      "let main = 2 * (4.6 - 3.1)" --> "2.999999999999999"

   it "Eq instances" $ do
      "let main = 2 == 3" --> "false"
      "let main = 2 /= 3" --> "true"
      "let main = 2 == 2" --> "true"
      "let main = 2 /= 2" --> "false"
      "let main = 'a' /= 'b'" --> "true"
      "let main = 'a' == 'b'" --> "false"


   it "Comparisons operators" $ do
      "let main = 2 < 3" --> "true"
      "let main = 2 > 3" --> "false"
      "let main = 2 >= 2" --> "true"
      "let main = 4 <= 3" --> "false"

   it "Lambda many tuples pattern" $ do
        [i|
            let f (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
            let main = f (2, 3) (5, 6)
        |] --> "[7,9]"

   it "Floating tests" $ do
      "let main = cos 1.5" --> "0.0707372016677029"
      "let main = sin 0.5" --> "0.479425538604203"
      "let main = sqrt 4"  --> "2"

   it "Integral tests" $ do
      "let main = mod 205 54" --> "43"

   it "Complex numbers" $ do 
      "let main = (2, 3) + (5, 6)" --> "[7,9]"
      "let main = (3, 9) - (2, 5)" --> "[1,4]"
      "let main = (7, 3) * (2, 5)" --> "[-1,41]"
      "let main = (2, 3) + 1" --> "[3,4]"
      "let main = 1 + (2, 3)" --> "[3,4]"
      "let main = (2, 3) - 1" --> "[1,2]"

   it "List syntax" $ do
      "let main = []" --> "[]"
      "let main = [1]" --> "[1]"
      "let main = [1,2,3]" --> "[1,2,3]"
      "let main = [1,2,3,4]" --> "[1,2,3,4]"
      "let main = [\'h\', \'e\']" --> "[\"h\",\"e\"]"
      "let main = [1 + 2, 3 * 4]" --> "[3,12]"
      "let main = [[1,2],[3,5]]" --> "[[1,2],[3,5]]"

   it "Construct fix type value" $ [i|
      let main = In (Cons 5 (In (Cons 4 (In Empty))))
   |] --> "{\"value0\":{\"value0\":5,\"value1\":{\"value0\":{\"value0\":4,\"value1\":{\"value0\":{}}}}}}"

   it "Catamorphism product" $ [i|
      let fix f x = f (fix f) x
      let cata psi f = psi . fmap f . out
      let cataRec psi = fix (cata psi)
      let example = In (Cons 5 (In (Cons 4 (In (Cons 3 (In (Cons 2 (In (Cons 1 (In Empty))))))))))
      let alg v = if isEmpty v then 1  
            else if isCons v then 
            let (x, y) = extractCons v in x * y
            else undefined
      let main = cataRec alg example
   |] --> "120"

   it "Maybe basics" $ do 
      "let main = Just 2" --> "{\"value\":2}"
      "let main = Nothing" --> "{}"
      [i|let f x = Just (x + 1)
         let main = f 5|] --> "{\"value\":6}"

   it "applicative operator" $ do
      "let main = [(\\x -> x + 1)] <*> [5]" --> "[6]"

   it "Maybe classes" $ do 
      "let main = fmap (\\x -> x * x) (Just 3)" --> "{\"value\":9}"
      [i|let f m n = bind m (\\x -> 
                     bind n (\\y ->
                     pure (x + y)))
         let main = f (Just 3) (Just 4)|] --> "{\"value\":7}"
      "let main = Just (\\x -> x * x) <*> Just 5" --> "{\"value\":25}"

   it "List and Maybe combined" $ do
      [i|val f :: (a -> b) -> Maybe (List a) -> Maybe (List b)
         let f = fmap . fmap
         
         let main = f (\\x -> x * 3) (Just [1,2,3])  |] --> "{\"value\":[3,6,9]}"

   it "Traversable example" $ do
      "let main = traverse (\\x -> [x * 2, x * 3]) (Just 5)" --> "[{\"value\":10},{\"value\":15}]"
      "let main = traverse Just [1,2,3,4]" --> "{\"value\":[1,2,3,4]}"
   
   it "Parser Test 3" $ "tests/jnrs_lib/parser_example3.jnr" ---> "[[[1,2,-5,-3,7],[]]]"

   it "Example 2" $ "tests/jnrs_lib/example2.jnr" ---> "[[[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14]],[[250,176,14],[250,176,14],[250,176,14],[250,176,14],[233,209,36],[233,209,36],[233,209,36],[233,209,36],[233,209,36],[233,209,36]],[[250,176,14],[250,176,14],[250,176,14],[233,209,36],[207,234,67],[207,234,67],[207,234,67],[233,209,36],[233,209,36],[233,209,36]],[[250,176,14],[250,176,14],[207,234,67],[207,234,67],[174,250,103],[137,255,141],[229,214,41],[137,255,141],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[207,234,67],[63,232,210],[63,232,210],[229,214,41],[229,214,41],[229,214,41],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[229,214,41],[229,214,41],[229,214,41],[229,214,41],[229,214,41],[13,61,231],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[207,234,67],[63,232,210],[63,232,210],[229,214,41],[229,214,41],[229,214,41],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[207,234,67],[207,234,67],[174,250,103],[137,255,141],[229,214,41],[137,255,141],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[250,176,14],[233,209,36],[207,234,67],[207,234,67],[207,234,67],[233,209,36],[233,209,36],[233,209,36]],[[250,176,14],[250,176,14],[250,176,14],[250,176,14],[233,209,36],[233,209,36],[233,209,36],[233,209,36],[233,209,36],[233,209,36]],[[250,176,14]]]"

   it "Colors Test" $ "tests/jnrs_lib/color_conversion.jnr" ---> "[[[255,0,0],[0,255,0],[0,0,255]],[[127,127,127],[127,0,0],[255,127,127]]]"
   
   it "Plasma Test" $ "tests/jnrs_lib/plasma_example.jnr" ---> "[[[255,0,0],[255,1,0],[255,2,0],[255,3,0],[255,5,0]],[[255,1,0],[255,2,0],[255,3,0],[255,4,0],[255,5,0]],[[255,2,0],[255,3,0],[255,4,0],[255,5,0],[255,6,0]],[[255,3,0],[255,4,0],[255,5,0],[255,6,0],[255,7,0]],[[255,4,0],[255,4,0],[255,5,0],[255,6,0],[255,7,0]],[[255,5,0]]]"

   it "Calculator Test" $ "tests/jnrs_lib/calculator.jnr" ---> "[[5,[]]]"

   it "Calculator Test 2" $ "tests/jnrs_lib/calculator_with_spaces.jnr" ---> "[[14,[]]]"

   it "Duality of Sorts" $ "tests/jnrs_lib/duality_of_sorts.jnr" ---> "[0,1,3,5,6,9]"