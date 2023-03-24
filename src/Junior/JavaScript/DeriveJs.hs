{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Junior.JavaScript.DeriveJs where

import Junior.Core.Types (Type (TyApp, TyCon))
import Data.Text (Text, pack)
import Data.String.Interpolate (i)
import Junior.Pretty.TypesPrinter ()

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
deriveConstructor _ (TyApp (TyCon n) (TyCon n2)) = pack [i|
    if (m instanceof __#{n}) {
        return m;
    };
|]
deriveConstructor tf (TyApp (TyCon n) t) | t == tf = pack [i|
    if (m instanceof __#{n}) {
              return applyClosure(#{n}, applyClosure(env["f"], applyClosure(extract#{n}, m)));
    };
|]
deriveConstructor tf (TyApp (TyApp (TyCon n) t2) t3) | t2 == tf && t3 == tf = pack [i|
    if (m instanceof __#{n}) {
              const [x, y] = applyClosure(extract#{n}, m);
              return applyClosure(applyClosure(#{n}, applyClosure(env["f"], x)), applyClosure(env["f"], y));
    };
|]
deriveConstructor tf (TyApp (TyApp (TyApp (TyCon n) t2) t3) t4) | t2 == tf && t3 == tf && t4 == tf = pack [i|
    if (m instanceof __#{n}) {
              const [x, y, z] = applyClosure(extract#{n}, m);
              return applyClosure(applyClosure(applyClosure(#{n}, applyClosure(env["f"], x)), applyClosure(env["f"], y)), applyClosure(env["f"], z));
    };
|]
deriveConstructor tf (TyApp (TyApp (TyApp (TyCon n) _) t3) t4) | t3 == tf && t4 == tf = pack [i|
    if (m instanceof __#{n}) {
              const [x, y, z] = applyClosure(extract#{n}, m);
              return applyClosure(applyClosure(applyClosure(#{n}, x), applyClosure(env["f"], y)), applyClosure(env["f"], z));
    };
|]
deriveConstructor tf (TyApp (TyApp (TyCon n) _) t2) | t2 == tf = pack [i|
    if (m instanceof __#{n}) {
              const [x, y] = applyClosure(extract#{n}, m);
              return applyClosure(applyClosure(#{n}, x), applyClosure(env["f"], y));
    };
|]
deriveConstructor tf (TyApp (TyApp (TyApp (TyCon n) _) _) t3) | t3 == tf = pack [i|
    if (m instanceof __#{n}) {
              const [x, y, z] = applyClosure(extract#{n}, m);
              return applyClosure(applyClosure(applyClosure(#{n}, x), y), applyClosure(env["f"], z));
    };
|]
deriveConstructor tf (TyApp (TyCon n) (TyApp (TyApp (TyCon "->") t1) t2)) | t2 == tf = pack [i|
    if (m instanceof __#{n}) {
              return applyClosure(#{n}, function (x) { return applyClosure(env["f"], applyClosure(extract#{n}, m)(x))}); 
    };
|]
deriveConstructor tf (TyApp (TyApp (TyApp (TyCon n) _) (TyApp (TyCon t2) t3)) (TyApp (TyCon t4) t5)) | t3 == tf && t5 == tf = pack [i|
    if (m instanceof __#{n}) {
        const [x, left, right] = applyClosure(extract#{n}, m);
        return applyClosure(applyClosure(applyClosure(#{n}, applyClosure(env["f"], x)), applyClosure(applyClosure(functor#{t2}.fmap, env["f"]), left)), applyClosure(applyClosure(functor#{t4}.fmap, env["f"]), right));
    };
|]
deriveConstructor t1 t2 = pack [i|// Implement deriveFunctor (#{show t1}) (#{show t2})|]


