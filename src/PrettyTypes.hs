{-# LANGUAGE TupleSections #-}
module PrettyTypes where

import Fixpoint (Fix(..))
import Ast (ExpF(..))
import Annotations (Ann(..), mapAnnM)
import TypedAst (TypedExp)
import Types ( Type(..), Qual(..), Pred(..) )
import Control.Monad ( unless )
import Control.Monad.Trans.State ( evalState, get, put, State )
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set as Set ( fromList, toList, Set )

type PrettyM = State (Map.Map String Char, Char)

getName :: String -> PrettyM Char
getName k = do success <- containsKey
               unless success addName
               (m, _) <- get
               return $ fromJust (Map.lookup k m)
            where
                containsKey :: PrettyM Bool
                containsKey = do (m, _) <- get
                                 return (Map.member k m)

                addName :: PrettyM ()
                addName = do (m, i) <- get
                             put (Map.insert k i m, toEnum (fromEnum i + 1))

runT :: Type -> PrettyM Type
runT (TyVar name k) = do newName <- getName name
                         return (TyVar [newName] k)
runT (TyApp t1 t2) = do t1' <- runT t1
                        t2' <- runT t2
                        return (TyApp t1' t2')

runT (TyCon name) = return (TyCon name)

runP :: Pred -> PrettyM Pred
runP (IsIn n t) = fmap (IsIn n) (runT t)

runPs :: Set.Set Pred -> PrettyM (Set.Set Pred)
runPs ps = fmap Set.fromList (mapM runP (Set.toList ps))

pretty :: Type -> Type
pretty t = evalState (runT t) (Map.empty, 'a')

prettyQM :: Qual Type -> PrettyM (Qual Type)
prettyQM (ps :=> t) = do ps' <- runPs ps
                         t'  <- runT t
                         return (ps' :=> t')

prettyQ :: Qual Type -> Qual Type
prettyQ qt = evalState (prettyQM qt) (Map.empty, 'a')

prettifyTypesM :: TypedExp -> PrettyM TypedExp
prettifyTypesM (In (Ann (loc, qt) (Let n v b))) = do
     qt' <- prettyQM qt 
     put (Map.empty, 'a')
     n' <- prettifyTypesM n
     v' <- prettifyTypesM v
     b' <- prettifyTypesM b
     return $ In (Ann (loc, qt') (Let n' v' b'))
prettifyTypesM e = 
    mapAnnM (\(loc, qt) -> (loc,) <$> prettyQM qt) e

prettifyTypes :: TypedExp -> TypedExp 
prettifyTypes e = evalState (prettifyTypesM e) (Map.empty , 'a')
