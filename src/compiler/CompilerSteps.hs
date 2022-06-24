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
import JavascriptGenerator (generate, generateData)
import qualified ClosureConversion (convertProg)
import qualified ANFTranslation (convertProg)
import qualified OptimizeTypeClasses (optimize)
import qualified DeadCodeElimination (optimize)
import qualified OptimizeClosureEnvs (optimize)
import SynExpToTypeDecl (toTypeDecl, fromTypeDeclToEnv, fromTypeDeclToClassEnv)
import Data.Maybe (maybeToList)
import qualified DesugarPatternMatching (desugar)

parse :: String -> CompileM [SynExp]
parse code =
   case parseExpr code of
       Left s -> throwError s
       Right v -> return v

fromSynExpToDataDecl :: [SynExp] -> CompileM [SynExp]
fromSynExpToDataDecl es = do
    let typeDecls' = es >>= toTypeDecl 
    (classEnv, env, symbols, typeDecls) <- get
    let env' = foldr concatEnvs env $ fromTypeDeclToEnv <$> typeDecls'
    let classEnv' = foldr fromTypeDeclToClassEnv classEnv typeDecls'
    put (classEnv', env', symbols, typeDecls <> typeDecls')
    return $ es

fromSynExpToExp :: [SynExp] -> CompileM [Exp]
fromSynExpToExp es = return $ es >>= (maybeToList . toExp)

dependencyAnalysis :: [Exp] -> CompileM [[(String, Exp)]]
dependencyAnalysis es = do
    (_, env, _, _) <- get
    let ns = (fst <$>) <$> chunks (Map.keysSet env) es
    let dict = Map.fromList ((\e -> case e of
         (In (Ann _ (Defn _ (In (Ann _ (VarPat s))) _))) -> (s, e)
         _ -> error "Not a definition.") <$> es)
    return $ ((\n -> (n, (Map.!) dict n)) <$>) <$> ns

typeInference :: [[(String, Exp)]] -> CompileM [TypedExp]
typeInference bss = do
    (classEnv, env, symbols, typeDecls) <- get
    let g (_, env') (n, e) = (\e2@(In (Ann (_, t) _)) -> ([e2], toEnv [(n, t)])) . snd <$> (infer classEnv env' . liftN) e
    let f env' (n, e) = env' >>= flip g (n, e)
    let k ev1 ev2 = (\(e1, x) (e2, y) -> (e1 ++ e2, concatEnvs x y)) <$> ev1 <*> ev2
    let h acc bs = foldl k acc (f acc <$> bs)
    let ret = foldl h (Right ([], env)) bss
    case ret of
        Left err -> throwError err
        Right v -> do
             put (classEnv, snd v, symbols, typeDecls)
             return $ fst v

buildSymbolTable :: [TypedExp] -> CompileM [TypedExp]
buildSymbolTable es = 
    do (classEnv, env, _, typeDecls) <- get
       put (classEnv, env, build (prettifyTypes <$> es), typeDecls)
       return es

prettyPrintModule :: [TypedExp] -> CompileM Text
prettyPrintModule es = return $ typedModuleToString es

desugarPredicates :: [TypedExp] -> CompileM [TypedExp]
desugarPredicates es = do
    (classEnv, env, _, _) <- get
    return $ convertPreds classEnv env <$> es

desugarPatternMatching :: [TypedExp] -> CompileM [TypedExp]
desugarPatternMatching es = 
    return $ DesugarPatternMatching.desugar es

interpret :: [TypedExp] -> CompileM [Result]
interpret es = do
   (_, env) <- ask
   case interpretModule env (mapAnn fst <$> es) of
       Left err -> throwError err
       Right v -> 
           return $ if member "it" v
           then Maybe.maybeToList $ lookup "it" v
           else Maybe.maybeToList $ lookup "main" v

toJs :: [TypedExp] -> CompileM Text
toJs xs = do
    (_, _, _, dataDecls) <- get
    let dataJs = generateData dataDecls
    let js = generate xs
    return (dataJs <> "\r\n" <> js)

closureConversion :: [TypedExp] -> CompileM [TypedExp]
closureConversion es = do 
    (name, _) <- ask
    (_, env, _, _) <- get
    return $ ClosureConversion.convertProg name (keysSet env) es

aNormalisation :: [TypedExp] -> CompileM [TypedExp]
aNormalisation es = do 
    return $ ANFTranslation.convertProg es

optimizeTypeClasses :: [TypedExp] -> CompileM [TypedExp]
optimizeTypeClasses es = do
    return $ OptimizeTypeClasses.optimize es

deadCodeElimin :: [TypedExp] -> CompileM [TypedExp]
deadCodeElimin es = do
    return $ DeadCodeElimination.optimize es

optimizeClosureEnvs :: [TypedExp] -> CompileM [TypedExp]
optimizeClosureEnvs es = do
    return $ OptimizeClosureEnvs.optimize es