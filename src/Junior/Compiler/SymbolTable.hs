module Junior.Compiler.SymbolTable where

import Junior.Parser.Primitives ( primToStr )
import Junior.Pretty.TypesPrinter ()
import Junior.Core.Ast (ExpF(..))
import Junior.Utils.Fixpoint (Fix(..))
import Junior.Utils.Annotations (Ann(..))
import Junior.TypeChecker.TypedAst ( TypedExp, TypedExpF )
import Junior.Parser.Location (PString(..))
import Junior.Core.Types ( Qual, Type )
import Junior.Utils.RecursionSchemes ( cataRec )
import Data.Map (Map, (!?), fromList, union)
import Data.List (nubBy)
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
extractSymbolsFromLet (In (Ann (_, qt) (Defn _ n _))) = do
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
alg (Ann (loc, qt) (ConPat n xs)) = do
    xs' <- sequence xs
    return $ In (Ann (loc, qt) (ConPat n xs'))
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
alg (Ann (loc, qt) (Match e1 es)) = do
    e1' <- e1
    es' <- sequence es
    return $ In (Ann (loc, qt) (Match e1' es'))
alg (Ann (loc, qt) (MatchExp e1 e2)) = do
    e1' <- e1
    let names = fromList $ extractSymbols e1'
    e2' <- local (`union` names) e2
    return $ In (Ann (loc, qt) (MatchExp e1' e2'))
alg (Ann (loc, qt) (Defn qt' n v)) = do
    n' <- n
    let names = fromList $ extractSymbols n'
    v' <- local (`union` names) v
    return $ In (Ann (loc, qt) (Defn qt' n' v'))
alg _ = undefined

fromBinding :: Map String Symbol -> TypedExp -> [Symbol]
fromBinding env e = snd $ runWriter $ runReaderT (cataRec alg e) env

build :: [TypedExp] -> [Symbol]
build es = nubBy (\x1 x2 -> name x1 == name x2) list
    where dict = es >>= extractSymbolsFromLet
          topLevel = Set.fromList (name . snd <$> dict)
          list = do s <- es >>= fromBinding (fromList dict)
                    if name s `Set.member` topLevel then
                        return Symbol { name = name s, ty = ty s, parent = parent s, top = True }
                    else return s



