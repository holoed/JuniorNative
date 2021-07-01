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

instance Semigroup PString where
 (PStr (x, p1)) <> (PStr (y, _)) = PStr (x ++ y, p1) 

instance Monoid PString where
    mempty  = PStr ("", Nothing)

instance Show PString where
  show (PStr (x, p)) = x ++ " " ++ show p
    
newtype PString = PStr (String, Maybe Loc) deriving (Generic, NFData)
