{-# LANGUAGE QuasiQuotes #-}
module CompilePatternMatchingSpec where

import Data.String.Interpolate ( i )
import Test.Hspec ( describe, it, shouldBe, Spec, parallel, Expectation )
import Intrinsics (classEnv, env)
import InterpreterMonad (empty)
import CompilerMonad (run, CompileM)
import TypedAst (TypedExp)
import SynExpToExp ( fromExp )
import PrettyPrinter (prettyPrint)
import Annotations (mapAnn)
import Data.Char (isSpace)
import CompilerSteps (desugarPatternMatching, compilePatternMatching)
import Compiler (step, frontEnd)
import Control.Monad ((>=>))

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

(-->) :: String -> String -> Expectation
(-->) x y = (filter (\v -> (/=' ') v && (/='\n') v) x) `shouldBe` (filter (\v -> (/=' ') v && (/='\n') v) y) 

spec :: Spec
spec = parallel $
  describe "Compile_Pattern_Matching_Tests" $ do

    it "pattern match 0" $ do
      xs <- process "let foo x = match x with y -> y"
      unlines xs --> "let foo x = (matchFn(((\\y -> True, \\_v-> let y = _v in y)) : [])) x"

    it "pattern match 1" $ do
      let code = [i|
        data Option a = Some a | None
        let foo x = match x with | Some v -> v + 1
      |]
      xs <- process code
      unlines xs --> 
        "let foo x = matchFn((isSome,\\_v -> let v = extractSome _v in v + fromInteger 1) : []) x"

    it "pattern match 2" $ do
      let code = [i|
        data Option a = Some a | None
        let foo x = match x with 
                    | Some (Some v) -> v + 1
                    | Some None -> 1
                    | None -> 0
      |]
      xs <- process code
      unlines xs --> 
        [i|let foo x = matchFn((isSome,\\_v -> let ___patV0 = extractSome _v in 
                                               matchFn((isSome,\\_v -> let v = extractSome _v in v + fromInteger 1):
                                                       (isNone,\\_v -> fromInteger 1) : []) ___patV0):
                               (isNone,\\_v -> fromInteger 0):[])x|]

    it "pattern match 3" $ do
      let code = [i|
        data Option a = Some a | None
        let foo x = match x with 
                    | Some v -> v + 1
                    | None -> 0
      |]
      xs <- process code
      unlines xs --> [i|
         let foo x = matchFn((isSome,\\_v -> let v = extractSome _v in v + fromInteger 1):
                             (isNone,\\_v -> fromInteger 0):[])x |]

    it "pattern match 4" $ do
      let code = [i|
        data Option a = None | Some a
        let foo v = match v with 
                    | Some (Some x) -> x + 1
      |]
      xs <- process code
      unlines xs --> 
        [i|let foo v = matchFn((isSome,\\_v -> let ___patV0 = extractSome _v in matchFn((isSome,\\_v -> let x = extractSome _v in x + fromInteger1):[]) ___patV0):[])v|]

    it "pattern match 5" $ do
      let code = [i|
        let foo v = match v with 
                    | (x, y) -> x + y
      |]
      xs <- process code
      unlines xs --> 
        [i|let foo v = (matchFn(((\\_v -> let(___patV0,___patV1) = _v in((\\x -> True) ___patV0)&&((\\y -> True) ___patV1),
                                  \\_v-> let x = fst _v in let y = snd _v in x + y)):[]))v|]

    it "pattern match 6" $ do
      let code = [i|
        data Option a = None | Some a
        let foo v = match v with 
                    | (None, Some x) -> x
      |]
      xs <- process code
      unlines xs --> 
        [i|  let foo v = matchFn((\\_v -> let (___patV0,___patV1) = _v in isNone ___patV0 && isSome ___patV1,
                                  \\_v -> let x = extractSome (snd _v) in x):[])v|]

    it "pattern matching 7" $ do
      let code = [i|
        data ListF a b = Empty | Cons a b
         let foo v = 
            match v with
            | (Cons a Empty) -> Cons a Empty
         let main = foo (Cons (Cons Empty) Empty)
      |]
      xs <- process code
      unlines xs --> 
        [i|letfoov=matchFn((isCons,\\_v->let(a,___patV0)=extractCons_vinmatchFn((isEmpty,\\_v->ConsaEmpty):[])___patV0):[])vletmain=foo(Cons(ConsEmpty)Empty)|]

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
      xs <- process code
      unlines xs --> [i|
        letswapv=(matchFn(((isEmpty,\\_v->Empty)):(isCons,\\_v->let___patV1=extractCons_vin(matchFn(((\\_v->let(___patV0,___patV1)=_vin((\\a->True)___patV0)&&isEmpty___patV1,\\_v->leta=fst_vinConsaEmpty)):(((\\_v->let(___patV2,___patV3)=_vin((\\a->True)___patV2)&&isCons___patV3,\\_v->leta=fst_vinlet(b,x)=extractCons(snd_v)inifa<=bthenConsa(Consbx)elseConsb(Consax))):[])))___patV1):[]))v|]

   