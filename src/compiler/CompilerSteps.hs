{-# LANGUAGE OverloadedStrings #-}
module CompilerSteps where

import Fixpoint ( Fix(..) )
import Annotations (mapAnn, Ann(Ann) )
import TypedAst (TypedExp)
import Ast (Exp, ExpF(..))
import PAst (SynExp)
import CompilerMonad (CompileM)
import Parser ( parseExpr )
import SynExpToExp ( toExp )
import Data.Map as Map ( keysSet, fromList, (!) )
import DependencyAnalysis ( chunks )
import Environment (toEnv, concatEnvs)
import Infer ( infer )
import LiftNumbers ( liftN )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.Reader ( MonadReader(ask) )
import Control.Monad.State (MonadState(put, get))
import SymbolTable ( build )
import PrettyTypes ( prettifyTypes ) 
import ModulePrinter (typedModuleToString)
import ConstraintsResolution (convertPreds)
import Interpreter (interpretModule)
import InterpreterMonad (Result, member, lookup)
import qualified Data.Maybe as Maybe
import Prelude hiding (lookup)
import Data.Text (Text)
import JavascriptGenerator (generate)
import ClosureConversion (convertProg)

parse :: String -> CompileM [SynExp]
parse code =
   case parseExpr code of
       Left s -> throwError s
       Right v -> return v

fromSynExpToExp :: [SynExp] -> CompileM [Exp]
fromSynExpToExp es = return $ toExp <$> es

dependencyAnalysis :: [Exp] -> CompileM [[(String, Exp)]]
dependencyAnalysis es = do
    (env, _) <- get
    let ns = (fst <$>) <$> chunks (Map.keysSet env) es
    let dict = Map.fromList ((\e@(In (Ann _ (Defn _ (In (Ann _ (VarPat s))) _))) -> (s, e)) <$> es)
    return $ ((\n -> (n, (Map.!) dict n)) <$>) <$> ns

typeInference :: [[(String, Exp)]] -> CompileM [TypedExp]
typeInference bss = do
    (_, classEnv) <- ask
    (env, symbols) <- get
    let g (_, env') (n, e) = (\e2@(In (Ann (_, t) _)) -> ([e2], toEnv [(n, t)])) . snd <$> (infer classEnv env' . liftN) e
    let f env' (n, e) = env' >>= flip g (n, e)
    let k ev1 ev2 = (\(e1, x) (e2, y) -> (e1 ++ e2, concatEnvs x y)) <$> ev1 <*> ev2
    let h acc bs = foldl k acc (f acc <$> bs)
    let ret = foldl h (Right ([], env)) bss
    case ret of
        Left err -> throwError err
        Right v -> do
             put (snd v, symbols)
             return $ fst v

buildSymbolTable :: [TypedExp] -> CompileM [TypedExp]
buildSymbolTable es = 
    do (env, _) <- get
       put (env, build (prettifyTypes <$> es))
       return es

prettyPrintModule :: [TypedExp] -> CompileM Text
prettyPrintModule es = return $ typedModuleToString es

desugarPredicates :: [TypedExp] -> CompileM [TypedExp]
desugarPredicates es = do
    (_, classEnv) <- ask
    (env, _) <- get
    return $ convertPreds classEnv env <$> es

interpret :: [TypedExp] -> CompileM [Result]
interpret es = do
   (env, _) <- ask
   case interpretModule env (mapAnn fst <$> es) of
       Left err -> throwError err
       Right v -> 
           return $ if member "it" v
           then Maybe.maybeToList $ lookup "it" v
           else Maybe.maybeToList $ lookup "main" v

toJs :: [TypedExp] -> CompileM Text
toJs = return . generate

closureConversion :: [TypedExp] -> CompileM [TypedExp]
closureConversion es = do 
    (env, _) <- get
    return $ convertProg (keysSet env) es
