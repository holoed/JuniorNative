{-# LANGUAGE QuasiQuotes #-}
module UnitTests.CompilePatternMatchingSpec where

import Data.String.Interpolate ( i )
import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel )
import Junior.Compiler.Intrinsics (classEnv, env)
import Junior.Interpreter.InterpreterMonad (empty)
import Junior.Compiler.CompilerMonad (run, CompileM)
import Junior.TypeChecker.TypedAst (TypedExp)
import Junior.Parser.SynExpToExp ( fromExp )
import Junior.Pretty.Printer (prettyPrint)
import Junior.Utils.Annotations (mapAnn)
import Data.Char (isSpace)
import Junior.Compiler.CompilerSteps (desugarPatternMatching, compilePatternMatching)
import Junior.Compiler.Compiler (step, frontEnd)
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO)

closed :: String -> CompileM [TypedExp]
closed = frontEnd >=> 
  step "desugar pattern matching" desugarPatternMatching >=>
  step "compile pattern matching" compilePatternMatching  

process :: String -> IO [String]
process code = do
    (ret, _, _) <- run (closed code) ("", empty) (classEnv, env, [], [])
    return $ either (error . show) (toString <$>) ret

toString :: TypedExp -> String
toString = prettyPrint . fromExp . mapAnn fst

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

(-->) :: MonadThrow m => String -> String -> m ()
(-->) x y = (filter (\v -> (/=' ') v && (/='\n') v) x) `shouldBe` (filter (\v -> (/=' ') v && (/='\n') v) y) 

tests :: TopSpec
tests = parallel $
  describe "Compile_Pattern_Matching_Tests" $ do

    it "pattern match 0" $ do
      xs <- liftIO $ process "let foo x = match x with y -> y"
      unlines xs --> "let foo x0 = (matchFn(((\\y1 -> True, \\_v-> let y1 = _v in y1)) : [])) x0"

    it "pattern match 1" $ do
      let code = [i|
        data Option a = Some a | None
        let foo x = match x with | Some v -> v + 1
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        "let foo x0 = matchFn((isSome,\\_v -> let v1 = extractSome _v in v1 + fromInteger 1) : []) x0"

    it "pattern match 2" $ do
      let code = [i|
        data Option a = Some a | None
        let foo x = match x with 
                    | Some (Some v) -> v + 1
                    | Some None -> 1
                    | None -> 0
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|let foo x0 = matchFn((isSome,\\_v -> let ___patV0 = extractSome _v in 
                                               matchFn((isSome,\\_v -> let v1 = extractSome _v in v1 + fromInteger 1):
                                                       (isNone,\\_v -> fromInteger 1) : []) ___patV0):
                               (isNone,\\_v -> fromInteger 0):[])x0|]

    it "pattern match 3" $ do
      let code = [i|
        data Option a = Some a | None
        let foo x = match x with 
                    | Some v -> v + 1
                    | None -> 0
      |]
      xs <- liftIO $ process code
      unlines xs --> [i|
         let foo x0 = matchFn((isSome,\\_v -> let v1 = extractSome _v in v1 + fromInteger 1):
                             (isNone,\\_v -> fromInteger 0):[])x0 |]

    it "pattern match 4" $ do
      let code = [i|
        data Option a = None | Some a
        let foo v = match v with 
                    | Some (Some x) -> x + 1
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|let foo v0 = matchFn((isSome,\\_v -> let ___patV0 = extractSome _v in matchFn((isSome,\\_v -> let x1 = extractSome _v in x1 + fromInteger1):[]) ___patV0):[])v0|]

    it "pattern match 5" $ do
      let code = [i|
        let foo v = match v with 
                    | (x, y) -> x + y
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|let foo v0 = (matchFn(((\\_v -> let(___patV0,___patV1) = _v in((\\x1 -> True) ___patV0)&&((\\y2 -> True) ___patV1),
                                  \\_v-> let x1 = fst _v in let y2 = snd _v in x1 + y2)):[]))v0|]

    it "pattern match 6" $ do
      let code = [i|
        data Option a = None | Some a
        let foo v = match v with 
                    | (None, Some x) -> x
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|  let foo v0 = matchFn((\\_v -> let (___patV0,___patV1) = _v in isNone ___patV0 && isSome ___patV1,
                                  \\_v -> let x1 = extractSome (snd _v) in x1):[])v0|]

    it "pattern matching 7" $ do
      let code = [i|
        data ListF a b = Empty | Cons a b
         let foo v = 
            match v with
            | (Cons a Empty) -> Cons a Empty
         let main = foo (Cons (Cons Empty) Empty)
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|letfoov0=matchFn((isCons,\\_v->let(a1,___patV0)=extractCons_vinmatchFn((isEmpty,\\_v->Consa1Empty):[])___patV0):[])v0letmain=foo(Cons(ConsEmpty)Empty)|]

    it "pattern match 8" $ do
      let code = [i|
        data ListF a b = Empty | Cons a b

        let swap v = 
          match v with
          | Empty -> Empty
          | (Cons a Empty) -> Cons a Empty
          | (Cons a (Cons b x)) -> if a <= b 
                                    then Cons a (Cons b x)
                                    else Cons b (Cons a x)
      |]
      xs <- liftIO $ process code
      unlines xs --> [i|
        letswapv0=(matchFn(((isEmpty,\\_v->Empty)):(isCons,\\_v->let___patV1=extractCons_vin(matchFn(((\\_v->let(___patV0,___patV1)=_vin((\\a1->True)___patV0)&&isEmpty___patV1,\\_v->leta1=fst_vinConsa1Empty)):(((\\_v->let(___patV2,___patV3)=_vin((\\a2->True)___patV2)&&isCons___patV3,\\_v->leta2=fst_vinlet(b3,x4)=extractCons(snd_v)inifa2<=b3thenConsa2(Consb3x4)elseConsb3(Consa2x4))):[])))___patV1):[]))v0|]

    it "pattern match 9" $ do
      let code = [i|
        data Point = Point0D Int | Point2D Int Int | Point3D Int Int Int

        let toString v = 
          match v with
          | Point0D x -> show x
          | Point2D x y -> show x <> " " <> show y
          | Point3D x y z -> show x <> " " <> show y <> " " <> show z
      |]
      xs <- liftIO $ process code
      unlines xs --> [i|
        lettoStringv0=matchFn((isPoint0D,\\_v->letx1=extractPoint0D_vinshowx1):(isPoint2D,\\_v->let(x2,y3)=extractPoint2D_vinshowx2<>\"\"<>showy3):(isPoint3D,\\_v->let(x4,y5,z6)=extractPoint3D_vinshowx4<>\"\"<>showy5<>\"\"<>showz6):[])v0|]
   
    it "pattern match 10" $ do
      let code = [i|
        data BackProp = BackProp (List (List Double)) -- as 
                                 (List (List Double)) -- w(l+1) weights
                                 (List Double)        -- deltas 
                                 (List Double)        -- desired output 

        let toTuple v = 
          match v with
          | BackProp x1 x2 x3 x4 -> (x1, x2, x3, x4) 
      |]
      xs <- liftIO $ process code
      unlines xs --> [i|
        lettoTuplev0=matchFn((isBackProp,\\_v->let(x11,x22,x33,x44)=extractBackProp_vin(x11,x22,x33,x44)):[])v0|]
   