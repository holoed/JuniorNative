{-# LANGUAGE QuasiQuotes #-}
module ModuleTypeInferenceTests where

import SynExpToExp ( toExp )
import LiftNumbers ( liftN )
import Infer ( infer )
import Parser ( parseExpr )
import Types ( Pred(..), Qual(..), Type(..) )
import Environment ( toEnv, Env )
import Substitutions ( Substitutions )
import DagBindings (chunks)
import Modules (bindingsDict)
import Data.Set as Set (Set, fromList )
import Data.String.Interpolate ( i )
import Data.Map.Strict (keys, (!))
import Data.Bifunctor ( Bifunctor(second) )
import Test.Hspec ( SpecWith, describe, it, shouldBe, Expectation )

tyLam :: Type -> Type -> Type
tyLam t1 = TyApp (TyApp (TyCon "->") t1)

env :: Env
env = toEnv [("id", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
            ("==", Set.fromList [IsIn "Eq" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("-",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("+",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("*",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("/",  Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            (">",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("<",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("fst", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "a" 0)),
            ("snd", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "b" 0)),
            ("isEmpty", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Bool")),
            ("empty", Set.fromList [] :=> TyApp (TyCon "List") (TyVar "a" 0)),
            ("hd", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
            ("tl", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
            ("cons", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
            ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyCon "Int") (TyVar "a" 0)),
            ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyCon "Double") (TyVar "a" 0))]

classEnv :: [Qual Pred]
classEnv = [
  Set.fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Eq" (TyVar "b" 0)] :=> IsIn "Eq" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
  ]

globals :: Set String
globals = fromList $ keys env

process :: String -> String
process input = do
  let ast = parseExpr input
  let ty = ast >>= infer classEnv env . liftN . toExp . Prelude.head
  either id (show . snd) ty

typeOf :: [String] -> Either String (Substitutions, Qual Type)
typeOf s = parseExpr (unlines s) >>= infer classEnv env . liftN . toExp . head

(-->) :: String -> [(String, String)] -> Expectation
(-->) x y = (second (either error (show . snd)) <$> ret)  `shouldBe` y
    where es = either error (toExp <$>) (parseExpr x)
          ns = (fst <$>) $ concat $ chunks globals es
          dict = bindingsDict es
          ret = (\n -> (n, (infer classEnv env . liftN . (dict!)) n)) <$> ns

tests :: SpecWith ()
tests =
  describe "Module Type Inference Tests" $

    it "Simple bindings" $
           [i| let x = 12
               let y = True |] --> [("y", "Bool"),
                                    ("x", "Num a => a")]