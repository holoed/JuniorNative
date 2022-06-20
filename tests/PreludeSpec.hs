{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module PreludeSpec where

import Test.Hspec (Spec, shouldBe, describe, it, Expectation, parallel)
import Data.Text ( pack, unpack, Text )
import JavaScriptRunner (runJS)
import Junior (prelude, buildAll)
import Data.String.Interpolate ( i )

exec :: Text -> IO String
exec = do
   let libPath = "src/javascript/baseClosedLib.js"
   runJS libPath . unpack

(-->) :: [(String, Text)] -> String -> Expectation
(-->) s1 s2 = do lib <- prelude
                 (Right (js, _)) <- buildAll lib s1 
                 ret <- exec js
                 ret `shouldBe` s2

spec :: Spec
spec = parallel $ do
  describe "Prelude tests" $ do

   it "identity" $ [("main", "let main = id 5")] --> "5"
   
   it "fst" $ [("main", "let main = fst (4, False)")] --> "4"

   it "snd" $ [("main", "let main = snd (2, True)")] --> "true"

   it "maybeToList" $ do
       [("main", "let main = maybeToList (Just 5)")] --> "[5]"
       [("main", "let main = maybeToList Nothing")] --> "[]"

   it "listToMaybe" $ do
       [("main", "let main = listToMaybe [5]")] --> "{\"value\":5}"
       [("main", "let main = listToMaybe []")] --> "{}"

   it "foldr1" $ do 
       [("main", "let main = foldr1 (*) [1,2,3,4,5]")] --> "120"
       [("main", "let main = foldr1 (*) (Just 5)")] --> "5"

   it "sum" $ do 
       [("main", "let main = sum [1,2,3,4,5]")] --> "15"
       [("main", "let main = sum (Just 5)")] --> "5"
       [("main", "let main = sum Nothing")] --> "0"

   it "product" $ do 
       [("main", "let main = product [1,2,3,4,5]")] --> "120"
       [("main", "let main = product (Just 5)")] --> "5"
       [("main", "let main = product Nothing")] --> "1"

   it "flattem" $ do
       [("main", "let main = flatten [[1,2],[3,4]]")] --> "[1,2,3,4]"
       [("main", "let main = flatten (Just (Just 4))")] --> "{\"value\":4}"

   it "sequenceA" $ do
       [("main", "let main = sequenceA [Just 'a', Just 'b']")] --> "{\"value\":[\"a\",\"b\"]}"
       [("main", "let main = sequenceA (Just [1,2,3,4])")] --> "[{\"value\":1},{\"value\":2},{\"value\":3},{\"value\":4}]"

   it "liftA3" $ do
       [("main", "let main = liftA3 (\\x y z -> (x, y, z)) (Just 1) (Just 'a') (Just False)")] --> "{\"value\":[1,\"a\",false]}"

   it "foldKliesli" $ do 
       [("main", "let main = (foldKliesli [\\x -> [x + 1, x - 1], \\x -> [x * 2.0, x / 2.0]]) 5")] --> "[12,3,8,2]"

   it "foldl" $ do 
       [("main", "let main = foldl (flip (:)) [] [1,2,3,4,5]")] --> "[5,4,3,2,1]"

   it "min and max" $ do 
       [("main", "let main = min 3 4")] --> "3"
       [("main", "let main = max 3 4")] --> "4"
       [("main", "let main = max 'a' 'c'")] --> "\"c\""
       [("main", "let main = min 'a' 'c'")] --> "\"a\""

   it "fix" $ do
       [("main", pack [i|
            let facRec f n = if n == 0 then 1 else n * f (n - 1)
            let fac = fix (facRec)
            let main = fac 5
       |])] --> "120"

   it "fmap operator" $ do
       [("main", "let main = ((+) 1) <$> [1,2,3,4]")] --> "[2,3,4,5]"

