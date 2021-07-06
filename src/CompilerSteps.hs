module CompilerSteps where

import Fixpoint ( Fix(..) )
import Annotations ( Ann(Ann) )
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
import SymbolTable ( Symbol, build )

parse :: String -> CompileM [SynExp]
parse code =
   case parseExpr code of
       Left s -> throwError s
       Right v -> return v

fromSynExpToExp :: [SynExp] -> CompileM [Exp]
fromSynExpToExp es = return $ toExp <$> es

dependencyAnalysis :: [Exp] -> CompileM [[(String, Exp)]]
dependencyAnalysis es = do
    env <- get
    let ns = (fst <$>) <$> chunks (Map.keysSet env) es
    let dict = Map.fromList ((\e@(In (Ann _ (Let (In (Ann _ (VarPat s))) _ _))) -> (s, e)) <$> es)
    return $ ((\n -> (n, dict!n)) <$>) <$> ns

typeInference :: [[(String, Exp)]] -> CompileM [TypedExp]
typeInference bss = do
    classEnv <- ask
    env <- get
    let g (_, env') (n, e) = (\e2@(In (Ann (_, t) _)) -> ([e2], toEnv [(n, t)])) . snd <$> (infer classEnv env' . liftN) e
    let f env' (n, e) = env' >>= flip g (n, e)
    let k ev1 ev2 = (\(e1, x) (e2, y) -> (e1 ++ e2, concatEnvs x y)) <$> ev1 <*> ev2
    let h acc bs = foldl k acc (f acc <$> bs)
    let ret = foldl h (Right ([], env)) bss
    case ret of
        Left err -> throwError err
        Right v -> do
             put (snd v)
             return $ fst v

buildSymbolTable :: [TypedExp] -> CompileM ([TypedExp], [Symbol])
buildSymbolTable es = pure (es, build es)

