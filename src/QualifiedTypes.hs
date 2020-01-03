module QualifiedTypes where

import Types
  
data Pred = IsIn String Type deriving (Eq, Show)

data Qual t = [Pred] :=> t deriving Eq
