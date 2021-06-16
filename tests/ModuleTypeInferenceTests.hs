{-# LANGUAGE QuasiQuotes #-}
module ModuleTypeInferenceTests where

import SynExpToExp ( toExp )
import LiftNumbers ( liftN )
import Infer ( infer )
import Parser ( parseExpr )
import Types ( Pred(..), Qual(..), Type(..), TypeScheme (ForAll) )
import Environment ( toEnv, Env )
import DagBindings (chunks)
import Modules (bindingsDict)
import Data.Set as Set (Set, fromList )
import Data.String.Interpolate ( i )
import Data.Map.Strict (keys, (!), union, toList, restrictKeys)
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
            ("null", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Bool")),
            ("empty", Set.fromList [] :=> TyApp (TyCon "List") (TyVar "a" 0)),
            ("hd", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
            ("tl", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
            ("cons", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
            ("singleton", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyApp (TyCon "List") (TyVar "a" 0))),
            ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyCon "Int") (TyVar "a" 0)),
            ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyCon "Double") (TyVar "a" 0))]

classEnv :: [Qual Pred]
classEnv = [
  Set.fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Eq" (TyVar "b" 0)] :=> IsIn "Eq" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
  ]

globals :: Set String
globals = fromList $ keys env

typeOfModule :: String -> [(String, String)] 
typeOfModule x = (\(n, ForAll _ qt) -> (n, show qt)) <$> toList ret 
    where es = either error (toExp <$>) (parseExpr x)
          ns = (fst <$>) $ concat $ chunks globals es
          dict = bindingsDict es
          bs = (\n -> (n, dict!n)) <$> ns
          f env' (n, e) = 
             let t = (either (\_ -> fromList [] :=> TyCon "error") snd . infer classEnv env' . liftN) e in
             toEnv [(n, t)] `union` env'    
          ret = restrictKeys (foldl f env bs) (fromList ns)

(-->) :: String -> [(String, String)] -> Expectation
(-->) x y = typeOfModule x `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Module Type Inference Tests" $ do

    it "Simple bindings" $
           [i| let x = 12
               let y = True |] --> [("x","Num a => a"),
                                    ("y","Bool")]

    it "Dependent bindings" $
           [i| let x = 12
               let y = (x + 1, True) |] --> [("x","Num a => a"),
                                             ("y","Num a => (a, Bool)")]

    it "Many nodes" $ [i|       
                      let foldr f v xs = 
                         if (null xs) then v 
                         else f (hd xs) (foldr f v (tl xs)) 

                      let concat xs ys = foldr cons ys xs
                     
                      let filter p = foldr (\\x -> \\xs -> if (p x) then cons x xs else xs) empty
                    
                      let quicksort f xs =
                        if (null xs) then xs else  
                        let pivot = hd xs in
                        let rest = tl xs in
                        let lessThan = filter (\\x -> f x < f pivot) rest in 
                        let greaterThan = filter (\\x -> f x > f pivot) rest in
                        concat (concat (quicksort f lessThan) (singleton pivot)) (quicksort f greaterThan)
                      |] --> [("concat","List a -> List a -> List a"),
                              ("filter","(a -> Bool) -> List a -> List a"),
                              ("foldr","(a -> b -> b) -> b -> List a -> b"),
                              ("quicksort","Ord a => (b -> a) -> List b -> List b")]
