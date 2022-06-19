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
deriveConstructor _ _ = "// TODO: Implement"