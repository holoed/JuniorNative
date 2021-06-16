module Modules where

import Ast ( Exp, ExpF(Let) )
import Fixpoint ( Fix(In) )
import Data.Map ( fromList, Map )

bindingsDict :: [Exp] -> Map String Exp 
bindingsDict es =  fromList ((\e@(In (Let s _ _)) -> (s, e)) <$> es)