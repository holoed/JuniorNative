{-# LANGUAGE QuasiQuotes #-}
module ModuleTypeInferenceTests where

import Types ( Pred(..), Qual(..), Type(..) )
import Environment ( toEnv, Env )
import Modules (typeOfModule)
import Data.Set as Set (fromList )
import Data.String.Interpolate ( i )
import Test.Hspec ( SpecWith, describe, it, shouldBe, Expectation )
import System.IO

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
            ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyCon "Int") (TyVar "a" 0)),
            ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyCon "Double") (TyVar "a" 0))]

classEnv :: [Qual Pred]
classEnv = [
  Set.fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Eq" (TyVar "b" 0)] :=> IsIn "Eq" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
  ]

(-->) :: String -> [(String, String)] -> Expectation
(-->) x y = typeOfModule classEnv env x `shouldBe` y

(--->) :: FilePath -> [(String, String)] -> Expectation 
(--->) x y = do handle <- openFile x ReadMode
                contents <- hGetContents handle
                contents --> y

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
                    
                      let singleton x = cons x empty

                      let quicksort f xs =
                        if (null xs) then xs else  
                        let pivot = hd xs in
                        let rest = tl xs in
                        let lessThan = filter (\\x -> f x < f pivot) rest in 
                        let greaterThan = filter (\\x -> f x > f pivot) rest in
                        concat (concat (quicksort f lessThan) (singleton pivot)) (quicksort f greaterThan)
                      |] --> [("foldr","(a -> b -> b) -> b -> List a -> b"),
                              ("concat","List a -> List a -> List a"),
                              ("filter","(a -> Bool) -> List a -> List a"),
                              ("singleton","a -> List a"),
                              ("quicksort","Ord a => (b -> a) -> List b -> List b")]

    it "Complex example" $ "tests/example.jnr" ---> [
      ("foldr","(a -> b -> b) -> b -> List a -> b"),
      ("concat","List a -> List a -> List a"),
      ("foldl","(a -> b -> a) -> a -> List b -> a"),
      ("join","List List a -> List a"),
      ("map","(a -> b) -> List a -> List b"),
      ("bind","(a -> List b) -> List a -> List b"),
      ("filter","(a -> Bool) -> List a -> List a"),
      ("product","Num a => List a -> a"),
      ("sum","Num a => List a -> a")
     ]
