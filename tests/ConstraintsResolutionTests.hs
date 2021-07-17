module ConstraintsResolutionTests where

import Types ( Pred(IsIn), Type(TyVar, TyCon) ) 
import ConstraintsResolution (typeForPred, toCamel, varNameForPred)
import Test.Hspec ( it, describe, shouldBe, SpecWith )
import TypesPrinter () 

tests :: SpecWith ()
tests = do

  describe "Constraints Resolution Tests" $ do

   it "Type for a Predicate" $ do
       let (-->) x y = (show . typeForPred) x `shouldBe` y
       IsIn "Num" (TyVar "a" 0) --> "Num a"
       IsIn "Monad" (TyVar "m" 1) --> "Monad m"
       IsIn "Num" (TyCon "Int") --> "Num Int"

   it "camel case names" $ do
       toCamel "FooBar" `shouldBe` "fooBar"

   it "VarName for a Predicate" $ do
       let (-->) x y = varNameForPred x `shouldBe` y
       IsIn "Num" (TyVar "a" 0) --> "numa0"
       IsIn "Num" (TyCon "Int") --> "numInt"
       IsIn "Monad" (TyCon "List") --> "monadList"
       IsIn "Monad" (TyVar "m" 1) --> "monadm1"