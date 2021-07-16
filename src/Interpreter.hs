module Interpreter where

import Location ( PString )
import Annotations ( unwrap )
import Ast (Exp, ExpF(..))
import Control.Monad.Reader ( MonadReader(ask, local), runReader, Reader )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import RecursionSchemes (cataRec)
import Data.Map ( Map, (!), insert, union )
import Primitives ( Prim(..) )
import Control.Monad.Fail ()

type Env = Map String Result
type InterpreterM = ExceptT PString (Reader Env) 

data Result = Value Prim
            | Function (Result -> InterpreterM Result)
 
interpret :: Env -> Exp -> Either PString Result
interpret env =  flip runReader env . runExceptT  . cataRec alg . unwrap
    where alg (Lit x) = return $ Value x
          alg (Var n) = do ctx <- ask
                           return $ ctx!n
          alg (VarPat n) = return $ Value (S n)
          alg (Lam e1 e2) = do ctx <- ask
                               nr <- e1
                               let (Value (S n)) = nr
                               return $ Function (\r -> local (\ctx' -> insert n r ctx' `union` ctx) e2)
          alg (Let e1 e2 e3) = do nr <- e1
                                  let (Value (S n)) = nr
                                  v' <- e2
                                  local (insert n v') e3
          alg (App e1 e2) = do rf <- e1
                               let (Function f) = rf
                               x <- e2
                               f x
          alg (IfThenElse e1 e2 e3) = do rp <- e1
                                         let (Value (B b)) = rp
                                         if b then e2 else e3
          alg _ = undefined



