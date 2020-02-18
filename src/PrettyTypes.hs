module PrettyTypes where

import Types
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Char (intToDigit, digitToInt)
import Data.Set as Set

getName :: String -> State (Map.Map String Char, Char) Char
getName k = do success <- containsKey
               unless success addName
               (m, _) <- get
               return $ fromJust (Map.lookup k m)
            where
                containsKey :: State (Map.Map String Char, Char) Bool
                containsKey = do (m, _) <- get
                                 return (Map.member k m)

                addName :: State (Map.Map String Char, Char) ()
                addName = do (m, i) <- get
                             put (Map.insert k i m, intToDigit (digitToInt i + 1))

runT :: Type -> State (Map.Map String Char, Char) Type
runT (TyVar name k) = do newName <- getName name
                         return (TyVar [newName] k)

runT (TyLam args body) = do argsAcc <- runT args
                            bodyAcc <- runT body
                            return (TyLam argsAcc bodyAcc)

runT (TyApp t1 t2) = do t1' <- runT t1
                        t2' <- runT t2
                        return (TyApp t1' t2')

runT (TyCon name) = return (TyCon name)

runP :: Pred -> State (Map.Map String Char, Char) Pred
runP (IsIn n t) = fmap (IsIn n) (runT t)

runPs :: Set.Set Pred -> State (Map.Map String Char, Char) (Set.Set Pred)
runPs ps = fmap (Set.fromList) (sequence (fmap runP (Set.toList ps)))

pretty :: Type -> Type
pretty t = evalState (runT t) (Map.empty, 'a')
        
prettyQ :: Qual Type -> Qual Type
prettyQ (ps :=> t) = evalState (do ps' <- runPs ps
                                   t' <- runT t 
                                   return (ps' :=> t')) (Map.empty, 'a')