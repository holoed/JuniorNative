{-# LANGUAGE QuasiQuotes #-}
module UnitTests.DeriveSpec where

import Test.Sandwich (TopSpec, shouldBe, describe, it, parallel)
import Data.String.Interpolate (i)
import Data.Text ( pack )
import Junior.JavaScript.DeriveJs (derive)
import Junior.Core.Types (Type(TyApp, TyCon, TyVar))


tests :: TopSpec
tests = parallel $ do
  describe "Derive Tests" $ do

    it "Derive Functor 0" $
        derive (TyApp (TyCon "FooF") (TyVar "a" 0)) 
               [TyApp (TyCon "FooF") (TyVar "a" 0)] "Functor" `shouldBe`
               pack [i|
const functorFooF = {
  \"fmap\": mkClosure(function ([_, f]) {
      return setEnv(\"f\", f, mkClosure(function ([env, m]) {
          
    if (m instanceof __FooF) {
              return applyClosure(FooF, applyClosure(env[\"f\"], applyClosure(extractFooF, m)));
    };

          throw new Error(\"Failed pattern match\");
      })); 
   })
}
|]

    it "Derive Functor 1" $
        derive (TyApp (TyCon "FooF") (TyVar "a" 0)) 
               [TyCon "Nil", 
                TyApp (TyApp (TyCon "Cons") (TyCon "Int")) (TyVar "a" 0)] "Functor" `shouldBe` 
                pack [i|
const functorFooF = {
  \"fmap\": mkClosure(function ([_, f]) {
      return setEnv(\"f\", f, mkClosure(function ([env, m]) {
          
    if (m instanceof __Nil) {
              return Nil;
    };

    if (m instanceof __Cons) {
              const [x, y] = applyClosure(extractCons, m);
              return applyClosure(applyClosure(Cons, x), applyClosure(env[\"f\"], y));
    };

          throw new Error(\"Failed pattern match\");
      })); 
   })
}
|]

