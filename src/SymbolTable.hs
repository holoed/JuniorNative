module SymbolTable where

import Primitives ( primToStr )
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
import qualified Data.Set as Set

type SymbolM = ReaderT (Map String Symbol) (Writer [Symbol])

data Symbol = Symbol { name:: PString,
                       ty :: Qual Type,
                       parent :: Maybe Symbol,
                       top :: Bool  } deriving (Eq, Show)

fromTypedExp :: String -> TypedExp -> (PString, Qual Type)
fromTypedExp s (In (Ann (loc, qt) _)) = (PStr (s, loc), qt)

extractSymbolsFromLet :: TypedExp -> [(String, Symbol)]
extractSymbolsFromLet (In (Ann (_, qt) (Let n _ _))) = do
  (n', s) <- extractSymbols n
  return (n', Symbol { name = name s, ty = qt, parent = Nothing, top = True })
extractSymbolsFromLet _ = []

extractSymbols :: TypedExp -> [(String, Symbol)]
extractSymbols (In (Ann (loc, qt) (VarPat s))) =
     [(s, Symbol { name = PStr(s, loc), ty = qt, parent = Nothing, top = False })]
extractSymbols (In (Ann _ (TuplePat xs))) = xs >>= extractSymbols
extractSymbols (In (Ann _ _) ) = []

alg :: TypedExpF (SymbolM TypedExp) -> SymbolM TypedExp
alg (Ann (loc, qt) (Lit v)) = do
    tell [Symbol { name = PStr(primToStr v, loc), ty = qt, parent = Nothing, top = False }]
    return $ In (Ann (loc, qt) (Lit v))
alg (Ann (loc, qt) (Var n)) = do
    env <- ask
    tell [Symbol { name = PStr(n, loc), ty = qt, parent = env!?n, top = False}]
    return $ In (Ann (loc, qt) (Var n))
alg (Ann (loc, qt) (VarPat n)) = do
    tell [Symbol { name = PStr(n, loc), ty = qt, parent = Nothing, top = False}]
    return $ In (Ann (loc, qt) (VarPat n))
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
    let names = fromList $ extractSymbols x'
    e' <- local (`union` names) e
    return $ In (Ann (loc, qt) (Lam x' e'))
alg (Ann (loc, qt) (Let n v b)) = do
    n' <- n
    let names = fromList $ extractSymbols n'
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
build es = list
    where dict = es >>= extractSymbolsFromLet
          topLevel = Set.fromList (name . snd <$> dict)
          list = do s <- es >>= fromBinding (fromList dict)
                    if name s `Set.member` topLevel then
                        return Symbol { name = name s, ty = ty s, parent = parent s, top = True }
                    else return s



