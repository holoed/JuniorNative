module PrettyTypes where

import Types
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Char (intToDigit, digitToInt)

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

pretty :: Type -> Type
pretty t = evalState (run t) (Map.empty, 'a')
        where run :: Type -> State (Map.Map String Char, Char) Type
              run (TyVar name) = do newName <- getName name
                                    return (TyVar [newName])

              run (TyLam args body) = do argsAcc <- run args
                                         bodyAcc <- run body
                                         return (TyLam argsAcc bodyAcc)

              run (TyCon name typeArgs) = do list <- mapM run typeArgs
                                             return (TyCon name list)
