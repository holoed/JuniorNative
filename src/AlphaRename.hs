module AlphaRename where

import Ast ( Exp, ExpF(Let, Lam, Var), var, lam, leT )
import Data.Map ( (!), empty, insert, lookup, member, Map )
import Data.Maybe ( fromMaybe )
import Control.Monad.Trans.Reader ( ask, local, ReaderT(runReaderT) )
import Control.Monad.State ( evalState, State, MonadState(put, get) )
import Fixpoint ( Fix(In) )
import RecursionSchemes ( cataRec )
import Prelude hiding (lookup)

type AlphaM = ReaderT (Map String String) (State (Int, Map String String))

newName :: String -> AlphaM String
newName s = do (index, names) <- get
               if member s names then
                 return $ names!s
               else do let s' = s ++ show index
                       let names' = insert s' s names
                       put (index + 1, names')
                       return s'

alg :: ExpF (AlphaM Exp) -> AlphaM Exp
alg (Lam x e) = do x' <- newName x
                   e' <- local (insert x x') e
                   return $ lam x' e'
alg (Var x) = do ctx <- ask
                 let x' = fromMaybe x (lookup x ctx)
                 return $ var x'
alg (Let n v b) = do n' <- newName n
                     v' <- local (insert n n') v
                     b' <- local (insert n n') b
                     return $ leT n' v' b'
alg x = fmap In (sequenceA x)

rename :: Exp -> Exp
rename e =
  evalState (runReaderT (cataRec alg e) empty) (0, empty)
