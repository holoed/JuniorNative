{-# LANGUAGE QuasiQuotes #-}
module ConstraintsResolutionTests where

import TypedAst (TypedExp)
import Types ( Pred(IsIn), Type(TyApp, TyVar, TyCon) ) 
import ConstraintsResolution (typeForPred, toCamel, varNameForPred, convertPreds)
import Test.Hspec ( it, describe, shouldBe, SpecWith )
import TypesPrinter () 
import Parser ( parseExpr )
import Data.Either (fromRight)
import Infer ( infer )
import Intrinsics ( classEnv, env )
import LiftNumbers ( liftN )
import SynExpToExp ( toExp )
import ModulePrinter ( typedModuleToString )
import Data.String.Interpolate ( i )

typeOf :: [String] -> [TypedExp]
typeOf s = fromRight [] (parseExpr (unlines s) >>=  
           (infer classEnv env . liftN . toExp . head) >>= (\(_, e) -> Right [e]))

tests :: SpecWith ()
tests = do

  describe "Constraints Resolution Tests" $ do

   it "Type for a Predicate" $ do
       let (-->) x y = (show . typeForPred) x `shouldBe` y
       IsIn "Num" (TyVar "a" 0) --> "Num a"
       IsIn "Monad" (TyVar "m" 1) --> "Monad m"
       IsIn "Num" (TyCon "Int") --> "Num Int"
       IsIn "Reader" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) --> "Reader (a, b)"

   it "camel case names" $ do
       toCamel "FooBar" `shouldBe` "fooBar"
       toCamel "helloWorld123" `shouldBe` "helloWorld123"

   it "VarName for a Predicate" $ do
       let (-->) x y = varNameForPred x `shouldBe` y
       IsIn "Num" (TyVar "a" 0) --> "numa0"
       IsIn "Num" (TyCon "Int") --> "numInt"
       IsIn "Monad" (TyCon "List") --> "monadList"
       IsIn "Monad" (TyVar "m" 1) --> "monadm1"

   it "Convert predicates" $ do
       let (-->) x y = typedModuleToString (convertPreds <$> typeOf x) `shouldBe` y
       ["let f x = x + 1"] --> [i|val f :: Num a -> a -> a
let f numT20 x = 
    (numT20 + x) (fromInteger numT20 1)
|]
       ["let f x y = (x + 1, y + 2)"] --> [i|val f :: Num a -> Num b -> a -> b -> (a, b)
let f numT50 numT60 x y = 
    ((numT50 + x) (fromInteger numT50 1), (numT60 + y) (fromInteger numT60 2))
|]