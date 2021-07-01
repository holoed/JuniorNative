{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Location where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Loc = Loc !Int  -- absolute character offset
               !Int  -- line number
               !Int  -- column number 
               deriving (Eq, Generic, NFData)

zeroLoc :: Loc
zeroLoc = Loc 0 0 0

instance Show Loc where 
    show (Loc _ l c) = "line " ++ show l ++ " column " ++ show c

instance Semigroup Loc where
   x <> y = x

type PString = (String, Maybe Loc)
