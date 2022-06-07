{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module PreludeSpec where

import Test.Hspec (Spec, shouldBe, describe, it, Expectation, parallel)
import Data.Text ( unpack, Text )
import JavaScriptRunner (runJS)
import Junior (prelude, buildAll)

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

   it "foldl" $ do 
       [("main", "let main = foldl (flip (:)) [] [1,2,3,4,5]")] --> "[5,4,3,2,1]"