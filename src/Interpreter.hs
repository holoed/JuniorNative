{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter where

import Location ( PString(..) )
import Annotations ( unwrap )
import Ast (Exp, ExpF(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader ( MonadReader(ask, local), runReader, Reader )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import RecursionSchemes (cataRec)
import Data.Map ( Map, (!), insert, union )
import Primitives ( Prim(..) )
import Control.Monad.Fail ()

type Env = Map String Result
newtype InterpreterM a = InterpreterM { runMonad :: ExceptT PString (Reader Env) a }
   deriving ( Functor, Applicative, Monad, MonadReader Env, MonadError PString )

instance MonadFail InterpreterM where
  fail s = throwError (PStr (s, Nothing)) 

data Result = Value Prim
            | Function (Result -> InterpreterM Result)
 
interpret :: Env -> Exp -> Either PString Result
interpret env =  flip runReader env . runExceptT  . runMonad .cataRec alg . unwrap
    where alg (Lit x) = return $ Value x
          alg (Var n) = do ctx <- ask
                           return $ ctx!n
          alg (VarPat n) = return $ Value (S n)
          alg (Lam e1 e2) = do ctx <- ask
                               (Value (S n))  <- e1
                               return $ Function (\r -> local (\ctx' -> insert n r ctx' `union` ctx) e2)
          alg (Let e1 e2 e3) = do (Value (S n)) <- e1
                                  v' <- e2
                                  local (insert n v') e3
          alg (App e1 e2) = do (Function f) <- e1
                               x <- e2
                               f x
          alg (IfThenElse e1 e2 e3) = do (Value (B b)) <- e1
                                         if b then e2 else e3
          alg _ = undefined



