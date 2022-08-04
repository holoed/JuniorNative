{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module UnitTests.PreludeSpec where

import Test.Sandwich (TopSpec, shouldBe, describe, it, parallel)
import Data.Text ( pack, unpack, Text )
import Junior.JavaScript.JavaScriptRunner (runJS)
import Junior.Compiler.Junior (prelude, buildAll)
import Data.String.Interpolate ( i )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Catch (MonadThrow)

exec :: Text -> IO String
exec = do
   let libPath = "src/Junior/JavaScript/baseClosedLib.js"
   runJS libPath . unpack

(-->) :: (MonadIO m, MonadThrow m, MonadFail m) => [(String, Text)] -> String -> m ()
(-->) s1 s2 = do lib <- liftIO $ prelude
                 (Right (js, _)) <- liftIO $ buildAll lib s1 
                 ret <- liftIO $ exec js
                 ret `shouldBe` s2

tests :: TopSpec
tests = parallel $ do
  describe "Prelude tests" $ parallel $ do

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

   it "groupBy adjacent elements according to some relation" $ do
       [("main", "let main = groupBy (\\x y -> x == y) [1,2,2,2,2,6,3,3,2,1,1,1]")] --> "[[1],[2,2,2,2],[6],[3,3],[2],[1,1,1]]"
       [("main", "let main = groupBy (\\x y -> x <= y) [1,2,2,3,1,2,0,4,5,2]")] --> "[[1,2,2,3],[1,2],[0,4,5],[2]]"

   it "partitionBy predicate" $ do
       [("main", "let main = partitionBy (\\x -> x <= 3) [1,2,3,4,5,6]")] --> "[[1,2,3],[4,5,6]]"

   it "sortBy predicate" $ do
       [("main", "let main = sortBy (\\x y -> x <= y) [4,3,2,7,5,9,4,0,8,5,4,4,3,2,6,4,3,3,3,7]")] --> "[0,2,2,3,3,3,3,3,4,4,4,4,4,5,5,6,7,7,8,9]"

   it "zipWith" $ do
       [("main", "let main = zipWith (+) [1,2,3] [4,5,6]")] --> "[5,7,9]"
       [("main", "let main = zipWith (\\x y -> (x, y)) [1,2,3,4,5] ['a','b','c','d']")] --> "[[1,\"a\"],[2,\"b\"],[3,\"c\"],[4,\"d\"]]"

   it "zipWithM" $ do
       [("main", "let main = zipWithM (\\x y -> Just (x, y)) [1,2,3] [4,5,6]")] --> "{\"value\":[[1,4],[2,5],[3,6]]}"
       [("main", "let main = zipWithM (\\x y -> [x, y]) [1,2,3] [4,5,6]")] --> "[[1,2,3],[1,2,6],[1,5,3],[1,5,6],[4,2,3],[4,2,6],[4,5,3],[4,5,6]]"

   it "replicate" $ do
       [("main", "let main = replicate 5 'a'")] -->  "[\"a\",\"a\",\"a\",\"a\",\"a\"]"

   it "liftA2" $ do
       [("main", "let main = liftA2 (\\x y -> (x, y)) (Just 3) (Just 5)")] --> "{\"value\":[3,5]}"
       [("main", "let main = liftA2 (\\x y -> [x, y]) [3] [5]")] --> "[[3,5]]"

   it "replicateM" $ do
       [("main", "let main = replicateM 5 (Just 'a')")] --> "{\"value\":[\"a\",\"a\",\"a\",\"a\",\"a\"]}"


