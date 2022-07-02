module UnitTests.AlphaRenameSpec where

import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel )
import AlphaRename ( rename )
import PrettyPrinter ( prettyPrint )
import Parser (parseExpr)
import SynExpToExp (fromExp, toExp)
import Data.Maybe (fromJust)
import Control.Monad.Catch (MonadThrow)

rn :: String -> String
rn s = either show (prettyPrint . (fromExp . rename . (fromJust . toExp) . head)) (parseExpr s)

(-->) :: MonadThrow m => String -> String -> m ()
(-->) x y = rn x `shouldBe` y

tests :: TopSpec
tests = parallel $
  describe "Alpha Rename Tests" $ do

     it "Rename identity" $
       "\\x -> x" --> "\\x0 -> x0"

     it "Rename simple lambda shadowing" $
       "\\x -> \\x -> x" --> "\\x0 x1 -> x1"

     it "Rename curry function" $
       "\\f -> \\x -> \\y -> f (x, y)" --> "\\f0 x1 y2 -> f0 (x1, y2)"

     it "Rename a tuple" $
       "\\x -> \\y -> (x, y)" --> "\\x0 y1 -> (x0, y1)"

     it "Rename a let" $ do
       "let x = 4" --> "let x0 = 4"
       "let f = \\x -> x in f 5" --> "let f0 x1 = x1 in f0 5"
