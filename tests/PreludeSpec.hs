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


