{-# LANGUAGE QuasiQuotes #-}
module UnitTests.ConstraintsResolutionSpec where

import Junior.Utils.Annotations (mapAnn)
import Junior.Core.Types ( Pred(IsIn), Type(TyApp, TyVar, TyCon), tyLam, Qual ((:=>)) ) 
import Junior.TypeChecker.ConstraintsResolution (typeForPred, toCamel, varNameForPred, getNewArgs)
import Test.Sandwich ( it, describe, shouldBe, TopSpec, parallel )
import Junior.Pretty.TypesPrinter () 
import Junior.Compiler.Intrinsics ( classEnv, env )
import Data.String.Interpolate ( i )
import Junior.Compiler.Compiler ( backendPrinted )
import Junior.Compiler.CompilerMonad ( run )
import Junior.Interpreter.InterpreterMonad (empty)
import Data.Text (unpack)
import Junior.Core.BuiltIns (tupleCon)
import Junior.Parser.SynExpToExp ( fromExp )
import Junior.Pretty.Printer ( prettyPrint )
import qualified Data.Set as Set
import Junior.TypeChecker.Environment (Env, concatEnvs, toEnv)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Cont (MonadIO(liftIO))

env' :: Env
env' = concatEnvs env $ toEnv [
  (".",  Set.fromList [] :=> tyLam (tyLam (TyVar "b" 0) (TyVar "c" 0))
                             (tyLam (tyLam (TyVar "a" 0) (TyVar "b" 0))
                             (tyLam (TyVar "a" 0) (TyVar "c" 0)))),
  ("id", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0))
 ]

build :: String -> IO String
build code = do
   (x, _, _) <- run (backendPrinted code) ("main", empty) (classEnv, env', [], [])
   return $ either show unpack x

(-->) :: (MonadIO m, MonadThrow m, MonadFail m) => String -> String -> m ()
(-->) s1 s2 = liftIO $ build s1 >>= (`shouldBe` s2)

tests :: TopSpec
tests = parallel $ do
  describe "Constraints Resolution Tests" $ do

   it "Type for a Predicate" $ do
       let (--->) x y = (show . typeForPred) x `shouldBe` y
       IsIn "Num" (TyVar "a" 0) ---> "Num a"
       IsIn "Monad" (TyVar "m" 1) ---> "Monad m"
       IsIn "Num" (TyCon "Int") ---> "Num Int"
       IsIn "Reader" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) ---> "Reader (a, b)"

   it "camel case names" $ do
       toCamel "FooBar" `shouldBe` "fooBar"
       toCamel "helloWorld123" `shouldBe` "helloWorld123"

   it "VarName for a Predicate" $ do
       let (--->) x y = varNameForPred x `shouldBe` y
       IsIn "Num" (TyVar "a" 0) ---> "numa"
       IsIn "Num" (TyCon "Int") ---> "numInt"
       IsIn "Monad" (TyCon "List") ---> "monadList"
       IsIn "Monad" (TyVar "m" 1) ---> "monadm"

   it "getNewArgs" $ do
       let testGetNewArgs xs = prettyPrint . fromExp . mapAnn fst <$> getNewArgs classEnv xs
       testGetNewArgs [IsIn "Eq" (TyCon "Int")] `shouldBe` ["eqInt"]   
       testGetNewArgs [IsIn "Eq" (tupleCon [TyCon "Int", TyCon "Int"])] `shouldBe` ["eqTuple2 eqInt eqInt"] 
       testGetNewArgs [IsIn "Eq" (tupleCon [tupleCon [TyCon "Int", TyCon "Int"], TyCon "Int"])] `shouldBe` ["eqTuple2 (eqTuple2 eqInt eqInt) eqInt"] 

   it "Convert predicates for Num function" $ 
       "let f x = x + 1" --> [i|val f :: Num a -> a -> a
let f numT2 x0 = 
    (numT2 + x0) (fromInteger numT2 1)
|]

   it "Convert predicates for curried function" $ 
       "let f x y = (x + 1, y + 2)" --> [i|val f :: Num a -> Num b -> a -> b -> (a, b)
let f numT5 numT6 x0 y1 = 
    ((numT5 + x0) (fromInteger numT5 1), (numT6 + y1) (fromInteger numT6 2))
|]

   it "Convert predicates for recursive function" $ 
       "let fac n = if n == 0 then 1 else n * (fac (n - 1))" --> 
           [i|val fac :: Eq a -> Num a -> a -> a
let fac eqT14 numT14 n0 = 
    if (eqT14 == n0) (fromInteger numT14 0)
        then fromInteger numT14 1
        else (numT14 * n0) (fac eqT14 numT14 ((numT14 - n0) (fromInteger numT14 1)))
|]

   it "Convert predicates for recursive function 2" $ 
       "let fib n = if n == 0 then 1 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)" --> 
           [i|val fib :: Eq a -> Num a -> Num b -> a -> b
let fib eqT28 numT28 numT3 n0 = 
    if (eqT28 == n0) (fromInteger numT28 0)
        then fromInteger numT3 1 else 
            if (eqT28 == n0) (fromInteger numT28 1)
                then fromInteger numT3 1
                else numT3 + fib eqT28 numT28 numT3 ((numT28 - n0) (fromInteger numT28 1)) (fib eqT28 numT28 numT3 ((numT28 - n0) (fromInteger numT28 2)))
|]

   it "Convert predicate for let value" $
       "let x = 42" --> [i|val x :: Int
let x = fromInteger numInt 42
|]

   it "Instance construction - Nested tuples" $
       "let main = ((1,2),3) == ((1,2), 3)" --> [i|val main :: Bool
let main = 
    eqTuple2 (eqTuple2 eqInt eqInt) eqInt == ((fromInteger numInt 1, fromInteger numInt 2), fromInteger numInt 3) (((fromInteger numInt 1, fromInteger numInt 2), fromInteger numInt 3))
|]

   it "Instance construction - Function Equality of Tuples" $
       "let f x y = (x, y) == (x, y)" --> [i|val f :: Eq a -> Eq b -> a -> b -> Bool
let f eqT8 eqT9 x0 y1 = 
    eqTuple2 eqT8 eqT9 == (x0, y1) (x0, y1)
|]

   it "Regression Test" $
    [i|
let reverse xs = foldl (\\xs x -> x : xs) [] xs

let partition n xs =
    let partition' n acc xs =
      if (n == 0 || null xs) then (reverse acc, xs)
          else partition' (n - 1) ((head xs) : acc) (tail xs) in
    partition' n [] xs|] --> [i|val reverse :: Foldable a -> a b -> List b
let reverse foldabletT6 xs0 = 
    foldl foldabletT6 (\\xs1 x2 ->
                       x2 : xs1) [] xs0

val partition :: Eq a -> Num a -> a -> List b -> (List b, List b)
let partition eqT44 numT44 n3 xs4 = 
    let partition'5 n6 acc7 xs8 = 
                if (eqT44 == n6) (fromInteger numT44 0) || null xs8
                    then (reverse foldableList acc7, xs8)
                    else ((partition'5 ((numT44 - n6) (fromInteger numT44 1))) (head xs8 : acc7)) (tail xs8) in
    partition'5 n3 [] xs4
|]

   it "Regression Test - Variable f shadowing top level decl f" $ [i|        
let map f xs = 
        if (null xs) then []
        else (f (head xs)) : map f (tail xs)
        
let xs = 1 : 2 : 3 : 4 : 5 : []

let f x = x + 1

let compose f g = f . g

let const x = \\v -> x

let main = (foldl compose f (map (const f) xs)) (5)   
|] --> [i|val compose :: (a -> b) -> (c -> a) -> c -> b
let compose f0 g1 = . f0 g1

val const :: a -> b -> a
let const x2 v3 = x2

val f :: Num a -> a -> a
let f numT2 x4 = \n    (numT2 + x4) (fromInteger numT2 1)

val map :: (a -> b) -> List a -> List b
let map f5 xs6 = if null xs6 then []
    else f5 (head xs6) : map f5 (tail xs6)

val xs :: List Int
let xs = 
    fromInteger numInt 1 : fromInteger numInt 2 : fromInteger numInt 3 : fromInteger numInt 4 : fromInteger numInt 5 : []

val main :: Int
let main = 
    foldl foldableList compose (f numInt) (map (const (f numInt)) xs) (fromInteger numInt 5)
|]

   it "Bug show list instance resolution" $
       "let x = show [5]" --> [i|val x :: String
let x = 
    show (showList showInt) (fromInteger numInt 5 : [])
|]