{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module DeriveJs where

import Types (Type (TyApp, TyCon))
import Data.Text (Text, pack)
import Data.String.Interpolate (i)

derive :: Type -> [Type] -> String -> Text
derive t ts "Functor" = deriveFunctor t ts
derive _ _ _ = "// not supported"

deriveFunctor :: Type -> [Type] -> Text
deriveFunctor (TyApp (TyCon n) tf) ts = pack [i|
const functor#{n} = {
  "fmap": mkClosure(function ([_, f]) {
      return setEnv("f", f, mkClosure(function ([env, m]) {
          #{(foldr (<>) "" . (deriveConstructor tf <$>)) ts}
          throw new Error("Failed pattern match");
      })); 
   })
}
|]
deriveFunctor (TyApp (TyApp (TyCon n) _) tf) ts = pack [i|
const functor#{n} = {
  "fmap": mkClosure(function ([_, f]) {
      return setEnv("f", f, mkClosure(function ([env, m]) {
          #{(foldr (<>) "" . (deriveConstructor tf <$>)) ts}
          throw new Error("Failed pattern match");
      })); 
   })
}
|]
deriveFunctor _ _ = ""

deriveConstructor :: Type -> Type -> Text
deriveConstructor _ (TyCon n) = pack [i|
    if (m instanceof __#{n}) {
              return #{n};
    };
|]
deriveConstructor tf (TyApp (TyCon n) t) | t == tf = pack [i|
    if (m instanceof __#{n}) {
              return applyClosure(#{n}, applyClosure(env["f"], applyClosure(extract#{n}, m)));
    };
|]
deriveConstructor tf (TyApp (TyApp (TyCon n) t1) t2) | t2 == tf = pack [i|
    if (m instanceof __#{n}) {
              const [x, y] = applyClosure(extract#{n}, m);
              return applyClosure(applyClosure(#{n}, x), applyClosure(env["f"], y));
    };
|]
deriveConstructor _ _ = "// TODO: Implement"