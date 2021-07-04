module CompilerSteps where

import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import Ast (Exp, ExpF(..))
import PAst (SynExp)
import CompilerMonad (CompileM)
import Parser ( parseExpr )
import SynExpToExp ( toExp )
import Data.Map as Map ( keysSet, fromList, toList, (!), restrictKeys )
import Data.Set as Set (fromList)
import Data.List (sortOn)
import DependencyAnalysis ( chunks )
import Monads (ask, throwError)
import Environment (Env, toEnv, concatEnvs)
import Infer ( infer )
import LiftNumbers ( liftN )
import Control.Parallel.Strategies (parMap, rdeepseq)
import Types (TypeScheme(..))

parse :: String -> CompileM [SynExp]
parse code =
   case parseExpr code of
       Left s -> throwError s
       Right v -> return v

fromSynExpToExp :: [SynExp] -> CompileM [Exp]
fromSynExpToExp es = return $ toExp <$> es

dependencyAnalysis :: [Exp] -> CompileM [[(String, Exp)]]
dependencyAnalysis es = do
    (_, env) <- ask
    let ns = (fst <$>) <$> chunks (Map.keysSet env) es
    let dict = Map.fromList ((\e@(In (Ann _ (Let (In (Ann _ (VarPat s))) _ _))) -> (s, e)) <$> es)
    return $ ((\n -> (n, dict!n)) <$>) <$> ns

typeInference :: [[(String, Exp)]] -> CompileM ([String], Env)
typeInference bss = do
    (classEnv, env) <- ask
    let g env' (n, e) = (\t -> toEnv [(n, t)]) . snd <$> (infer classEnv env' . liftN) e
    let f env' (n, e) = env' >>= flip g (n, e)
    let ret = foldl (\acc bs -> foldl (\ev1 ev2 -> concatEnvs <$> ev1 <*> ev2) acc $ parMap rdeepseq (f acc) bs) (Right env) bss
    case ret of
        Left err -> throwError err
        Right v -> return (((fst <$>) . concat) bss,  v)

fromEnvToTypeDict :: ([String], Env) -> CompileM [(String, String)]
fromEnvToTypeDict (ns, env) = do
     let finalEnv = restrictKeys env (Set.fromList ns)
     let unsortedRet = fmap (\ (n, ForAll _ qt) -> (n, show qt)) . toList $ finalEnv
     let orderDict = Map.fromList (zip ns ([1..] :: [Int]))
     return $ sortOn ((orderDict!) . fst) unsortedRet



