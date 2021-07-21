module AlphaRename where

import Location ( Loc )
import Ast ( Exp, ExpF(Let, Lam, Var), var, lam, leT, extractNameFromPat )
import Data.Map ( (!), empty, insert, lookup, member, Map )
import Data.Maybe ( fromMaybe, fromJust )
import Control.Monad.Trans.Reader ( ask, local, ReaderT(runReaderT) )
import Control.Monad.State ( evalState, State, MonadState(put, get) )
import Fixpoint ( Fix(In) )
import RecursionSchemes ( cataRec )
import Prelude hiding (lookup)
import Annotations (Ann(..))

type AlphaM = ReaderT (Map String String) (State (Int, Map String String))

newName :: String -> AlphaM String
newName s = do (index, names) <- get
               if member s names then
                 return $ names!s
               else do let s' = s ++ show index
                       let names' = insert s' s names
                       put (index + 1, names')
                       return s'

alg :: Ann (Maybe Loc) ExpF (AlphaM Exp) -> AlphaM Exp
alg (Ann (Just l) (Lam p e)) = do p' <- p
                                  let x = extractNameFromPat p'
                                  x' <- newName (snd x)                                   
                                  e' <- local (insert (snd x) x') e
                                  return $ lam l (var (fromJust $ fst x) x') e'
alg (Ann (Just l) (Var x)) = 
                  do ctx <- ask
                     let x' = fromMaybe x (lookup x ctx)
                     return $ var l x'
alg (Ann (Just l) (Let p v b)) = 
                  do p' <- p
                     let x = extractNameFromPat p'
                     x' <- newName (snd x) 
                     v' <- local (insert (snd x) x') v
                     b' <- local (insert (snd x) x') b
                     return $ leT l (var (fromJust $ fst x) x') v' b'
alg x = fmap In (sequenceA x)

rename :: Exp -> Exp
rename e =
  evalState (runReaderT (cataRec alg e) empty) (0, empty)
