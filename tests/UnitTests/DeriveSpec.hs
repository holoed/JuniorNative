{-# LANGUAGE QuasiQuotes #-}
module UnitTests.DeriveSpec where

import Test.Sandwich (TopSpec, shouldBe, describe, it, parallel)
import Data.String.Interpolate (i)
import Data.Text ( pack )
import Junior.JavaScript.DeriveJs (derive)
import Junior.Core.Types (Type(TyApp, TyCon, TyVar), tyLam)


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

    it "Derive Functor 2" $
        derive (TyApp (TyApp (TyCon "FooF") (TyVar "a" 0)) (TyVar "b" 0)) 
               [TyApp (TyCon "FooF") (tyLam (TyVar "a" 0) (TyVar "b" 0))] "Functor" `shouldBe` 
                pack [i|
const functorFooF = {
  \"fmap\": mkClosure(function ([_, f]) {
      return setEnv(\"f\", f, mkClosure(function ([env, m]) {
          
    if (m instanceof __FooF) {
              return applyClosure(FooF, function (x) { return applyClosure(env[\"f\"], applyClosure(extractFooF, m)(x))}); 
    };

          throw new Error(\"Failed pattern match\");
      })); 
   })
}
|]

    it "Derive Functor 3" $
        derive (TyApp (TyCon "Expr") (TyVar "a" 0)) 
               [TyApp (TyCon "Lit") (TyCon "Int"), 
                TyApp (TyCon "Var") (TyCon "String"),
                TyApp (TyCon "App") (tyLam (TyVar "a" 0) (TyVar "a" 0)),
                TyApp (TyCon "Lam") (tyLam (TyCon "String") (TyVar "a" 0))] "Functor" `shouldBe` 
                pack [i|
const functorExpr = {
  \"fmap\": mkClosure(function ([_, f]) {
      return setEnv(\"f\", f, mkClosure(function ([env, m]) {
          
    if (m instanceof __Lit) {
        return m;
    };

    if (m instanceof __Var) {
        return m;
    };

    if (m instanceof __App) {
              return applyClosure(App, function (x) { return applyClosure(env[\"f\"], applyClosure(extractApp, m)(x))}); 
    };

    if (m instanceof __Lam) {
              return applyClosure(Lam, function (x) { return applyClosure(env[\"f\"], applyClosure(extractLam, m)(x))}); 
    };

          throw new Error(\"Failed pattern match\");
      })); 
   })
}
|]

    it "Derive Functor 4" $
        derive (TyApp (TyCon "Expr") (TyVar "a" 0)) 
               [TyApp (TyApp (TyCon "App") (TyVar "a" 0)) (TyVar "a" 0),
                TyApp (TyApp (TyApp (TyCon "Let") (TyCon "String")) (TyVar "a" 0)) (TyVar "a" 0)] "Functor" `shouldBe` 
                pack [i|
const functorExpr = {
  \"fmap\": mkClosure(function ([_, f]) {
      return setEnv(\"f\", f, mkClosure(function ([env, m]) {
          
    if (m instanceof __App) {
              const [x, y] = applyClosure(extractApp, m);
              return applyClosure(applyClosure(App, applyClosure(env[\"f\"], x)), applyClosure(env[\"f\"], y));
    };

    if (m instanceof __Let) {
              const [x, y, z] = applyClosure(extractLet, m);
              return applyClosure(applyClosure(applyClosure(Let, x), applyClosure(env[\"f\"], y)), applyClosure(env[\"f\"], z));
    };

          throw new Error(\"Failed pattern match\");
      })); 
   })
}
|]

