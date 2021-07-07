module SymbolTable where

import TypesPrinter ()
import Ast (ExpF(..))
import Fixpoint (Fix(..))
import Annotations (Ann(..))
import TypedAst ( TypedExp, TypedExpF )
import Location (PString(..))
import Types ( Qual, Type )
import RecursionSchemes ( cataRec )
import Data.Map (Map, (!?), fromList, union)
import Control.Monad.Writer( Writer, MonadWriter(tell), runWriter )
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)

type SymbolM = ReaderT (Map String Symbol) (Writer [Symbol])

data Symbol = Symbol { name:: PString,
                       ty :: Qual Type,
                       parent :: Maybe Symbol,
                       top :: Bool  } deriving (Eq, Show)

fromTypedExp :: String -> TypedExp -> (PString, Qual Type)
fromTypedExp s (In (Ann (loc, qt) _)) = (PStr (s, loc), qt)

extractSymbols :: Bool -> TypedExp -> [(String, Symbol)]
extractSymbols tb (In (Ann (loc, qt) (VarPat s))) =
     [(s, Symbol { name = PStr(s, loc), ty = qt, parent = Nothing, top = tb })]
extractSymbols tb (In (Ann _ (TuplePat xs))) = xs >>= extractSymbols tb
extractSymbols tb (In (Ann (loc, qt) (Let n _ _))) = do
  (n', _) <- extractSymbols tb n
  return (n', Symbol { name = PStr (n', loc), ty = qt, parent = Nothing, top = tb })
extractSymbols _ (In (Ann _ _) ) = []

alg :: TypedExpF (SymbolM TypedExp) -> SymbolM TypedExp
alg (Ann (loc, qt) (Lit v)) = do
    tell [Symbol { name = PStr(show v, loc), ty = qt, parent = Nothing, top = False }]
    return $ In (Ann (loc, qt) (Lit v))
alg (Ann (loc, qt) (Var n)) = do
    env <- ask
    tell [Symbol { name = PStr(n, loc), ty = qt, parent = env!?n, top = False}]
    return $ In (Ann (loc, qt) (Var n))
alg (Ann (loc, qt) (VarPat n)) = do
    tell [Symbol { name = PStr(n, loc), ty = qt, parent = Nothing, top = False}]
    return $ In (Ann (loc, qt) (Var n))
alg (Ann (loc, qt) (MkTuple xs)) = do
    xs' <- sequence xs
    return $ In (Ann (loc, qt) (MkTuple xs'))
alg (Ann (loc, qt) (TuplePat xs)) = do
    xs' <- sequence xs
    return $ In (Ann (loc, qt) (TuplePat xs'))
alg (Ann (loc, qt) (App f x)) = do
    f' <- f
    In . Ann (loc, qt) . App f' <$> x
alg (Ann (loc, qt) (Lam x e)) = do
    x' <- x
    let names = fromList $ extractSymbols False x'
    e' <- local (`union` names) e
    return $ In (Ann (loc, qt) (Lam x' e'))
alg (Ann (loc, qt) (Let n v b)) = do
    n' <- n
    let names = fromList $ extractSymbols False n'
    v' <- local (`union` names) v
    b' <- local (`union` names) b
    return $ In (Ann (loc, qt) (Let n' v' b'))
alg (Ann (loc, qt) (IfThenElse p t h)) = do
    p' <- p
    t' <- t
    In . Ann (loc, qt) . IfThenElse p' t' <$> h

fromBinding :: Map String Symbol -> TypedExp -> [Symbol]
fromBinding env e = snd $ runWriter $ runReaderT (cataRec alg e) env

build :: [TypedExp] -> [Symbol]
build es = topLevel ++ (es >>= fromBinding (fromList dict))
    where dict = es >>= extractSymbols True
          topLevel = snd <$> dict

