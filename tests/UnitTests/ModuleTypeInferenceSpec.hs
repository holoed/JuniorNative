{-# LANGUAGE QuasiQuotes #-}
module UnitTests.ModuleTypeInferenceSpec where

import Data.String.Interpolate ( i )
import Test.Sandwich ( TopSpec, describe, it, shouldBe, parallel )
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Junior.Compiler.Intrinsics ( env, classEnv )
import Junior.Parser.Location ( PString, getName )
import Junior.Compiler.Compiler (frontEndPrinted)
import Junior.Compiler.CompilerMonad (run)
import qualified Junior.Compiler.SymbolTable as S
import Data.List (nub)
import Junior.Pretty.PrettyTypes (prettyQ)
import Junior.Interpreter.InterpreterMonad (empty) 
import Junior.TypeChecker.Environment (Env, concatEnvs, toEnv)
import Junior.Core.Types ( Type(..), Qual(..), tyLam, Pred (IsIn) )
import qualified Data.Set as Set
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Catch (MonadThrow)

env' :: Env
env' = concatEnvs env $ toEnv [
  ("out", Set.fromList [] :=> tyLam (TyApp (TyCon "Fix") (TyVar "f" 1)) (TyApp (TyVar "f" 1) (TyApp (TyCon "Fix") (TyVar "f" 1)))),
  ("In", Set.fromList [] :=> tyLam (TyApp (TyVar "f" 1) (TyApp (TyCon "Fix") (TyVar "f" 1))) (TyApp (TyCon "Fix") (TyVar "f" 1))),
  (".",  Set.fromList [] :=> tyLam (tyLam (TyVar "b" 0) (TyVar "c" 0))
                             (tyLam (tyLam (TyVar "a" 0) (TyVar "b" 0))
                             (tyLam (TyVar "a" 0) (TyVar "c" 0)))),
  ("id", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("fst", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "a" 0)),
  ("snd", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "b" 0)),
  ("zipWith", Set.fromList [] :=> tyLam (tyLam (TyVar "a" 0) (tyLam (TyVar "b" 0) (TyVar "c" 0))) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (tyLam (TyApp (TyCon "List") (TyVar "b" 0)) (TyApp (TyCon "List") (TyVar "c" 0))))),
  ("sum", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyCon "Double")) (TyCon "Double")),
  ("anaRec", Set.fromList [IsIn "Functor" (TyVar "f" 1)] :=> tyLam (tyLam (TyVar "a" 0) (TyApp (TyVar "f" 1) (TyVar "a" 0))) (tyLam (TyVar "a" 0) (TyApp (TyCon "Fix") (TyVar "f" 1)))),
  ("cataRec", Set.fromList [IsIn "Functor" (TyVar "f" 1)] :=> tyLam (tyLam (TyApp (TyVar "f" 1) (TyVar "a" 0)) (TyVar "a" 0)) (tyLam (TyApp (TyCon "Fix") (TyVar "f" 1)) (TyVar "a" 0))),
  ("<$>", Set.fromList [IsIn "Functor" (TyVar "f" 1)] :=> tyLam (tyLam (TyVar "a" 0) (TyVar "b" 0)) (tyLam (TyApp (TyVar "f" 1) (TyVar "a" 0)) (TyApp (TyVar "f" 1) (TyVar "b" 0))))
 ]

extractNames :: [S.Symbol] -> [(String, String)]
extractNames ss = (\s -> (getName $ S.name s, show $ prettyQ $ S.ty s)) <$> filter S.top ss

typeOfModule :: String -> IO (Either PString [(String, String)])
typeOfModule code = do
   (x, (_, _, ss, _), _) <- run (frontEndPrinted code) ("main", empty) (classEnv, env', [], [])
   return (nub . extractNames . const ss <$> x)

(-->) :: (MonadIO m, MonadThrow m, MonadFail m) => String -> [(String, String)] -> m ()
(-->) x y = do v <- liftIO $ typeOfModule x
               either (error . show) id v `shouldBe` y

(--->) :: (MonadIO m, MonadThrow m, MonadFail m) => FilePath -> [(String, String)] -> m ()
(--->) x y = do handle <- liftIO $ openFile x ReadMode
                contents <- liftIO $ hGetContents handle
                contents --> y

tests :: TopSpec
tests = parallel $
  describe "Module Type Inference Tests" $ do

    it "Not a binding" $ do
           [i| 42 |] --> [("it","Int")]
           [i| head |] --> [("it","List a -> a")]

    it "Simple bindings" $
           [i| let x = 12
               let y = True |] --> [("x","Int"),
                                    ("y","Bool")]

    it "Dependent bindings" $
           [i| let x = 12
               let y = (x + 1, True) |] --> [("x","Int"),
                                             ("y","(Int, Bool)")]

    it "Many nodes" $ [i|       
                      let foldright f v xs = 
                         if (null xs) then v 
                         else f (head xs) (foldright f v (tail xs)) 

                      let concat xs ys = foldright (\\x xs -> x : xs) ys xs
                     
                      let filter p = foldright (\\x -> \\xs -> if (p x) then x : xs else xs) []
                    
                      let singleton x = x : []

                      let quicksort f xs =
                        if (null xs) then xs else  
                        let pivot = head xs in
                        let rest = tail xs in
                        let lessThan = filter (\\x -> f x < f pivot) rest in 
                        let greaterThan = filter (\\x -> f x > f pivot) rest in
                        concat (concat (quicksort f lessThan) (singleton pivot)) (quicksort f greaterThan)
                      |] -->
                       [
                          ("foldright","(a -> b -> b) -> b -> List a -> b"),
                          ("singleton","a -> List a"),
                          ("concat","List a -> List a -> List a"),
                          ("filter","(a -> Bool) -> List a -> List a"),
                          ("quicksort","Ord a => (b -> a) -> List b -> List b")
                       ]

    it "Custom ADT" $
           [i| data Foo = Bar
               let x = Bar |] --> [("x","Foo")]

    it "Custom ADT match" $
           [i| data Foo = Bar
               let f = isBar |] --> [("f","Foo -> Bool")]

    it "Custom ADT 2" $
           [i| data Foo = Bar | Fuzz
               let x = Bar
               let y = Fuzz
               let z = (x, y) |] --> [("x","Foo"), ("y","Foo"), ("z","(Foo, Foo)")]
      
    it "Custom ADT 2 match" $
           [i| data Foo = Bar | Fuzz
               let f = (isBar, isFuzz) |] --> [("f","(Foo -> Bool, Foo -> Bool)")]

    it "Custom ADT with concrete types" $
           [i| data Pair = I Int | D Double
               let f = I
               let g = D |] --> [("f","Int -> Pair"), ("g","Double -> Pair")]

    it "Custom ADT with concrete types is match" $
           [i| data Pair = I Int | D Double
               let f = (isI, isD) |] --> [("f","(Pair -> Bool, Pair -> Bool)")]

    it "Custom ADT with concrete types extract" $
           [i| data Pair = I Int | D Double
               let f = (extractI, extractD) |] --> [("f","(Pair -> Int, Pair -> Double)")]

    it "Custom ADT with one type variable" $
           [i| data Option a = Some a | None
               let x = Some 5 |] --> [("x","Option Int")]

    it "Custom ADT with one type variable - is match" $
           [i| data Option a = Some a | None
               let f = (isSome, isNone)  |] --> [("f","(Option a -> Bool, Option b -> Bool)")]

    it "Custom ADT with one type variable - extract match" $
           [i| data Option a = Some a | None
               let f = extractSome  |] --> [("f","Option a -> a")]

    it "Custom ADT with two type variables - extract match" $
           [i|
            data L a b = E | C a b 

            let zero = In E

            let one = In (C 1 zero)

            let two = In (C 2 one)

            let main = two
          |] -->  [("zero","Fix (L a)"),("one","Fix (L Int)"),("two","Fix (L Int)"),("main","Fix (L Int)")]

    it "Pattern match 1" $ 
       [i|
              data ListF a b = Empty | Cons a b deriving Functor 

              let toListAlg v = 
              match v with
              | Empty -> []
              | Cons x xs -> x:xs 
       |] --> [("toListAlg","ListF a (List a) -> List a")]

    it "Complex example" $ "tests/jnrs_lib/example.jnr" ---> [
      ("++", "Foldable a => a b -> List b -> List b"), 
      ("cadd", "(Num a, Num b) => (a, b) -> (a, b) -> (a, b)"), 
      ("cmul", "Num a => (a, a) -> (a, a) -> (a, a)"), 
      ("filter", "Foldable a => (b -> Bool) -> a b -> List b"), 
      ("norm", "Num a => (a, a) -> a"), 
      ("map", "Foldable a => (b -> c) -> a b -> List c"), 
      ("mapM", "(Foldable a, Monad b) => (c -> b d) -> a c -> b (List d)"), 
      ("reverse", "Foldable a => a b -> List b"), 
      ("posToCoord", "Fractional a => a -> a -> (a, a)"), 
      ("product", "(Foldable a, Num b) => a b -> b"), 
      ("singleton", "a -> List a"), 
      ("range", "(Num a, Ord a) => (a -> b) -> a -> a -> List b"), 
      ("sum", "(Foldable a, Num b) => a b -> b"), 
      ("join", "Foldable a => a (List b) -> List b"), 
      ("mPoint", "(Eq a, Num a, Num b, Ord b) => a -> (b, b) -> (b, b) -> a"), 
      ("mandelbrot", "Integral a => Int -> (Int, Int) -> (a, a, a)"), 
      ("partition", "(Eq a, Num a) => a -> List b -> (List b, List b)"), 
      ("quicksort", "Ord a => (b -> a) -> List b -> List b"), 
      ("sequence", "(Foldable a, Monad b) => a (b c) -> b (List c)"), 
      ("split", "(Eq a, Num a) => a -> List b -> List (List b)")
     ]     

    it "Recursion schemes" $ "tests/jnrs_lib/RecursionSchemes.jnr" ---> [
      (".","(a -> b) -> (c -> a) -> c -> b"),
      ("ana","Functor a => (b -> a c) -> (c -> Fix a) -> b -> Fix a"),
      ("fix","((a -> b) -> a -> b) -> a -> b"),
      ("cata","Functor a => (a b -> c) -> (Fix a -> b) -> Fix a -> c"),
      ("hylo","Functor a => (b -> a c) -> (a d -> e) -> (c -> d) -> b -> e"),
      ("para","Functor a => (a ((Fix a, b)) -> c) -> (Fix a -> b) -> Fix a -> c"),
      ("anaRec","Functor a => (b -> a b) -> b -> Fix a"),
      ("cataRec","Functor a => (a b -> b) -> Fix a -> b"),
      ("hyloRec","Functor a => (b -> a b) -> (a c -> c) -> b -> c"),
      ("paraRec","Functor a => (a ((Fix a, b)) -> b) -> Fix a -> b")
     ]

    it "Custom ADT - Peano Numbers" $ "tests/jnrs_lib/peano_numbers.jnr" ---> [
       ("fromInt", "(Eq a, Num a) => a -> Nat"), 
       ("toInt", "Num a => Nat -> a"), 
       ("add", "Nat -> Nat -> Nat"), 
       ("mul", "Nat -> Nat -> Nat"), 
       ("facNat", "Nat -> Nat"), 
       ("fac", "Int -> Int"), 
       ("main", "Int")
     ]

    it "Folding neural networks test 2" $ "tests/jnrs_lib/folding_neural_networks_2.jnr" ---> [
       ("add", "List Double -> List Double -> List Double"), 
       ("mul", "List (List Double) -> List Double -> List Double"), 
       ("sigmoid", "List Double -> List Double"), 
       ("brain", "Fix Layer"), 
       ("algfwd", "Layer (List Double -> List (List Double)) -> List Double -> List (List Double)"), 
       ("backward", "List (List Double) -> List Double -> BackProp -> (List Double, List (List Double), List Double)"), 
       ("calc", "Fix Layer -> List Double -> List (List Int)"), ("getAs", "BackProp -> List (List Double)"), 
       ("getDesiredOutput", "BackProp -> List Double"), 
       ("coalgbwd", "(Fix Layer, BackProp) -> Layer ((Fix Layer, BackProp))"), 
       ("train", "(List Double, List Double) -> Fix Layer -> Fix Layer"), 
       ("newBrain", "Fix Layer"), 
       ("main", "List (List Int)")]



