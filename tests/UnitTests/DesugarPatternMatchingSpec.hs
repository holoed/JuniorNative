{-# LANGUAGE QuasiQuotes #-}
module UnitTests.DesugarPatternMatchingSpec where

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
import Junior.Compiler.CompilerSteps (desugarPatternMatching)
import Junior.Compiler.Compiler (step, frontEnd)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Catch (MonadThrow)

closed :: String -> CompileM [TypedExp]
closed = frontEnd >=> step "desugar pattern matching" desugarPatternMatching  

process :: String -> IO [String]
process code = do
    (ret, _, _) <- run (closed code) ("", empty) (classEnv, env, [], [])
    return $ either (error . show) (toString <$>) ret

toString :: TypedExp -> String
toString = prettyPrint . fromExp . mapAnn fst

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

(-->) :: (MonadIO m, MonadThrow m, MonadFail m) => String -> String -> m ()
(-->) x y = filter (\v -> (/=' ') v && (/='\n') v) x `shouldBe` filter (\v -> (/=' ') v && (/='\n') v) y

tests :: TopSpec
tests = parallel $
  describe "Desugar Pattern Matching Tests" $ do

    it "pattern match 0" $ do
      xs <- liftIO $ process "let foo x = match x with y -> y"
      unlines xs --> "let foo x0 = match x0 with y1 -> y1"

    it "pattern match 1" $ do
      let code = [i|
        data Option a = Some a | None
        let foo x = match x with | Some v -> v + 1
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        "let foo x0 = match x0 with Some v1 -> v1 + fromInteger 1"

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
        [i|let foo x0 = match x0 with 
                       Some ___patV0 -> match ___patV0 with 
                                        Some v1 -> v1 + fromInteger 1
                                       |None-> fromInteger 1
                      |None -> fromInteger 0|]

    it "pattern match 3" $ do
      let code = [i|
        data Option a = Some a | None
        let foo x = match x with 
                    | Some v -> v + 1
                    | None -> 0
      |]
      xs <- liftIO $ process code
      unlines xs --> [i|
         let foo x0 = match x0 with 
                      Some v1 -> v1 + fromInteger 1
                     |None   -> fromInteger 0 |]

    it "pattern match 4" $ do
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
        let swap v0 = match v0 with 
                     Empty -> Empty 
                    |Cons ___patV1 -> 
                       match ___patV1 with
                       (a1, Empty) -> Cons a1 Empty
                      |(a2, Cons b3 x4)-> if a2 <= b3
                                       then Cons a2 (Cons b3 x4)
                                       else Cons b3 (Cons a2 x4) |]
                                      
    it "pattern match 5" $ do
      let code = [i|
        data Option a = None | Some a
        let foo v = match v with 
                    | Some (Some x) -> x + 1
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|let foo v0 = match v0 with 
                       Some ___patV0 ->
                       match ___patV0 with Some x1 -> x1 + fromInteger 1|] 

    it "pattern match 6" $ do
      let code = [i|
        data ListF a b = Empty | Cons a b
         let foo v = 
            match v with
            | (Cons a Empty) -> Cons a Empty
         let main = foo (Cons (Cons Empty) Empty)
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|letfoov0=matchv0withConsa1___patV0->match___patV0withEmpty->Consa1Emptyletmain=foo(Cons(ConsEmpty)Empty)|]

    it "pattern match 7" $ do
      let code = [i|
        data BackProp = BackProp (List (List Double)) -- as 
                                 (List (List Double)) -- w(l+1) weights
                                 (List Double)        -- deltas 
                                 (List Double)        -- desired output 

        let main v = 
          match v with
          | BackProp x1 x2 x3 x4 -> (x1, x2, x3, x4) 
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|letmainv0=matchv0withBackPropx11x22x33x44->(x11,x22,x33,x44)|]

    it "pattern match 8" $ do 
      let code = [i|

        data Fix f = In (f (Fix f))

        data NatF a = Zero | Succ a deriving Functor

        let foo v = match v with  
          | Succ (In Zero) -> 42 
 
      
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|letfoov0=matchv0withSucc___patV0->match___patV0withIn___patV1->match___patV1withZero->fromInteger42|]

    it "pattern match 9" $ do 
      let code = [i|

        data Fix f = In (f (Fix f))

        data NatF a = Zero | Succ a deriving Functor

        let foo v = match v with  
          | Succ (In Zero) -> 42 
          | Succ y -> 12
 
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|letfoov0=matchv0withSucc___patV0->match___patV0withInZero->fromInteger42|y1->fromInteger12|]

    it "pattern match 10" $ do 
      let code = [i|
        data Exp = Var String
                | Lam String Exp
                | App Exp Exp
        
        val equal :: Exp -> Exp -> Bool 
        let equal x y = 
                  match (x, y) with
                  | (Var x, Var y) -> x == y
                  | (Lam s1 e1, Lam s2 e2) -> s1 == s2 && equal e1 e2
                  | (App e1 e2, App e3 e4) -> equal e1 e3 && equal e2 e4  
      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|letequal=\\x0y1->match(x0,y1)with(Varx2,Vary3)->x2==y3|(Lams14e15,Lams26e27)->s14==s26&&equale15e27|(Appe18e29,Appe310e411)->equale18e310&&equale29e411|]

    it "pattern match 11" $ do 
      let code = [i|
        data Prim = I Int | B Bool 

        let extract (I x) = x

      |]
      xs <- liftIO $ process code
      unlines xs --> 
        [i|let extract ___patV0 = match ___patV0 with I x0 -> x0|]
    

   