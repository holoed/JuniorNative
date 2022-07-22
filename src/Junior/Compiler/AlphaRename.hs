module Junior.Compiler.AlphaRename where

import Junior.Parser.Location ( Loc )
import Junior.TypeChecker.TypedAst (TypedExp, tlam, tvar, tleT, tmatchExp)
import Junior.Core.Ast ( Exp, ExpF(Let, Lam, Var, Defn, VarPat, TuplePat, MatchExp, ConPat) )
import Data.Map ( (!), empty, insert, lookup, member, Map, fromList )
import Data.Maybe ( fromMaybe )
import Control.Monad.Trans.Reader ( ask, local, ReaderT(runReaderT) )
import Control.Monad.State ( evalState, State, MonadState(put, get) )
import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Utils.RecursionSchemes ( cataRec )
import Prelude hiding (lookup)
import Junior.Utils.Annotations (Ann(..))
import Junior.Core.Types (Qual, Type)

type AlphaM = ReaderT (Map String String) (State (Int, Map String String))

newName :: String -> AlphaM String
newName s = do (index, names) <- get
               if member s names then
                 return $ names!s
               else do let s' = s ++ show index
                       let names' = insert s' s names
                       put (index + 1, names')
                       return s'

extractNameFromPat :: TypedExp -> AlphaM (Map String String, TypedExp) 
extractNameFromPat (In (Ann attr (VarPat s))) = do
  s' <- newName s
  return $ (fromList [(s,s')], (In (Ann attr (VarPat s'))))
extractNameFromPat (In (Ann attr (TuplePat ps))) = do
  psAndNames <- sequence (extractNameFromPat <$> ps)
  let (newNames, ps') = unzip psAndNames
  return $ (foldl (<>) empty newNames, (In (Ann attr (TuplePat ps'))))
extractNameFromPat (In (Ann attr (ConPat name ps))) = do
  psAndNames <- sequence (extractNameFromPat <$> ps)
  let (newNames, ps') = unzip psAndNames
  return $ (foldl (<>) empty newNames, (In (Ann attr (ConPat name ps'))))
extractNameFromPat _ = error "Unsupported"

alg :: Ann (Maybe Loc, Qual Type) ExpF (AlphaM TypedExp) -> AlphaM TypedExp
alg (Ann (Just l, qt) (Lam p e)) = 
  do p' <- p
     (newNames, p'') <- extractNameFromPat p'                            
     e' <- local (newNames <>) e
     return $ tlam l qt p'' e'
alg (Ann (Just l, qt) (Var x)) = 
  do ctx <- ask
     let x' = fromMaybe x (lookup x ctx)
     return $ tvar l qt x'
alg (Ann (Just l, qt) (Let p v b)) = 
  do p' <- p
     (newNames, p'') <- extractNameFromPat p'    
     v' <- local (newNames <>) v
     b' <- local (newNames <>) b
     return $ tleT l qt p'' v' b'
alg (Ann (Just l, qt) (MatchExp p e)) = 
  do p' <- p
     (newNames, p'') <- extractNameFromPat p'                            
     e' <- local (newNames <>) e
     return $ tmatchExp l qt p'' e'
alg x = fmap In (sequenceA x)

rename :: [TypedExp] -> [TypedExp]
rename e =
  evalState (runReaderT (sequence (cataRec alg <$> e)) empty) (0, empty)
