{-# LANGUAGE QuasiQuotes #-}
module UnitTests.CompileToJsSpec where

import Data.String.Interpolate ( i )
import Test.Sandwich (TopSpec, shouldBe, describe, it, parallel)
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Compiler ( fullJS )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack)
import JavaScriptRunner (runJS)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)

build :: String -> IO String
build code = do
   handle <- openFile "src/prelude/prelude.jnr" ReadMode 
   contents <- hGetContents handle
   let libPath = "src/javascript/baselib.js"
   (x, _, _) <- run (fullJS (contents <> "\r\n\r\n" <> code)) ("main", Interp.env) (classEnv, env, [], [])
   either (return . show) (runJS libPath . unpack) x

(-->) :: (MonadIO m, MonadThrow m, MonadFail m) => String -> String -> m ()
(-->) s1 s2 = liftIO $ build s1 >>= (`shouldBe` s2)

(--->) :: (MonadIO m, MonadThrow m, MonadFail m) => FilePath -> String -> m ()
(--->) x y = do handle <- liftIO $ openFile x ReadMode
                contents <- liftIO $ hGetContents handle
                contents --> y

tests :: TopSpec
tests = parallel $ do
  describe "Compile to JavaScript Tests" $ parallel $ do

   it "value" $ "let main = 42" --> "42"

   it "string value" $ "let main = \"Hello\"" --> "\"Hello\""

   it "char value" $ "let main = 'h'" --> "\"h\""

   it "applied function" $ [i|
      let f x = x + 1
      let main = f 5
   |] --> "6"

   it "Eq instances" $ do
      "let main = 2 == 3" --> "false"
      "let main = 2 /= 3" --> "true"
      "let main = 2 == 2" --> "true"
      "let main = 2 /= 2" --> "false"

   it "Comparisons operators" $ do
      "let main = 2 < 3" --> "true"
      "let main = 2 > 3" --> "false"
      "let main = 2 >= 2" --> "true"
      "let main = 4 <= 3" --> "false"

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

   it "Integral instances" $ do
      "let main = mod 5 3" --> "2"

   it "from List to Map" $ do
      "let main = fromListToMap (('a', 2):[])" --> "{\"a\":2}"

   it "Fix value level example" $ do [i|
      let fac f n = if n == 0 then 1 else n * f (n - 1)
      let facRec = fix fac
      let main = facRec 5|] --> "120"

   it "factorial example" $ [i|
     let fac n = if n == 0 then 1 else n * fac (n - 1)
     let main = fac 5
   |] --> "120"

   it "factorial with lists" $ [i|      
     let fac n = foldl (*) 1 (range (\\x-> x) 1 n)           
     let main = fac 5     
   |] --> "120"

   it "deconstucting tuples in lambdas" $ [i|
      let f (x, y) = x + y
      let main = f (2, 3)
   |] --> "5"

   it "deconstucting tuples in lets" $ [i|
      let add (z1, z2) = 
      let (a, b) = z1 in
      let (c, d) = z2 in
      (a + c, b + d)
      
      let main = add ((2, 3), (4, 5)) 
   |] --> "[6,8]"

   it "Async support" $ [i|
      val main :: Async Int 
      let main = (>>=) (pure 3) (\\x -> 
                 (>>=) (pure 2) (\\y -> pure (x + y)))    
   |] --> "5"

   it "List and Maybe combined" $ do
      [i|val f :: (a -> b) -> Maybe (List a) -> Maybe (List b)
         let f = fmap . fmap
         
         let main = f (\\x -> x * 3) (Just [1,2,3])  |] --> "{\"value\":[3,6,9]}"

   it "Parser Test 3" $ "tests/jnrs_lib/parser_example3.jnr" ---> "[[[1,2,-5,-3,7],\"\"]]"

   it "Calculator Test" $ "tests/jnrs_lib/calculator.jnr" ---> "[[5,\"\"]]"

   it "Calculator Test 2" $ "tests/jnrs_lib/calculator_with_spaces.jnr" ---> "[[14,\"\"]]"

   it "Example 2" $ "tests/jnrs_lib/example2.jnr" ---> "[[[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14],[250,176,14]],[[250,176,14],[250,176,14],[250,176,14],[250,176,14],[233,209,36],[233,209,36],[233,209,36],[233,209,36],[233,209,36],[233,209,36]],[[250,176,14],[250,176,14],[250,176,14],[233,209,36],[207,234,67],[207,234,67],[207,234,67],[233,209,36],[233,209,36],[233,209,36]],[[250,176,14],[250,176,14],[207,234,67],[207,234,67],[174,250,103],[137,255,141],[229,214,41],[137,255,141],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[207,234,67],[63,232,210],[63,232,210],[229,214,41],[229,214,41],[229,214,41],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[229,214,41],[229,214,41],[229,214,41],[229,214,41],[229,214,41],[13,61,231],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[207,234,67],[63,232,210],[63,232,210],[229,214,41],[229,214,41],[229,214,41],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[207,234,67],[207,234,67],[174,250,103],[137,255,141],[229,214,41],[137,255,141],[207,234,67],[233,209,36]],[[250,176,14],[250,176,14],[250,176,14],[233,209,36],[207,234,67],[207,234,67],[207,234,67],[233,209,36],[233,209,36],[233,209,36]],[[250,176,14],[250,176,14],[250,176,14],[250,176,14],[233,209,36],[233,209,36],[233,209,36],[233,209,36],[233,209,36],[233,209,36]],[[250,176,14]]]"

   it "Colors Test" $ "tests/jnrs_lib/color_conversion.jnr" ---> "[[[255,0,0],[0,255,0],[0,0,255]],[[127,127,127],[127,0,0],[255,127,127]]]"
   
   it "Plasma Test" $ "tests/jnrs_lib/plasma_example.jnr" ---> "[[[255,0,0],[255,1,0],[255,2,0],[255,3,0],[255,5,0]],[[255,1,0],[255,2,0],[255,3,0],[255,4,0],[255,5,0]],[[255,2,0],[255,3,0],[255,4,0],[255,5,0],[255,6,0]],[[255,3,0],[255,4,0],[255,5,0],[255,6,0],[255,7,0]],[[255,4,0],[255,4,0],[255,5,0],[255,6,0],[255,7,0]],[[255,5,0]]]"

   -- it "Duality of Sorts" $ "tests/jnrs_lib/duality_of_sorts.jnr" ---> "[0,1,3,5,6,9]"