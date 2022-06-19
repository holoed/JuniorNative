{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module DeriveJs where

import Types (Type (TyApp, TyCon))
import Data.Text (Text, pack)
import Data.String.Interpolate (i)

derive :: Type -> [Type] -> String -> Text
derive t ts "Functor" = deriveFunctor t ts
derive _ _ _ = "-- not supported"

deriveFunctor :: Type -> [Type] -> Text
deriveFunctor (TyApp (TyCon n) _) ts = pack [i|
const functor#{n} = {
  "fmap": mkClosure(function ([_, f]) {
      return setEnv("f", f, mkClosure(function ([env, m]) {
          #{(foldr (<>) "" . (deriveConstructor <$>)) ts}
          throw new Error("Failed pattern match");
      })); 
   })
}
|]

deriveConstructor :: Type -> Text
deriveConstructor t = "-- TODO: Implement"