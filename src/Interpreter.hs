{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter where

import Fixpoint (Fix(In))
import Location ( PString(..) )
import Annotations ( Ann(..) )
import Ast (Exp, ExpF(..), extractNameFromPat)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader ( MonadReader(ask, local), runReader, Reader )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import RecursionSchemes (cataRec)
import Data.Map ( Map, (!), insert, union, member )
import Primitives ( Prim(..) )
import Control.Monad.Fail ()
import Control.Monad (foldM)

type InterpreterEnv = Map String Result
newtype InterpreterM a = InterpreterM { runMonad :: ExceptT PString (Reader InterpreterEnv) a }
   deriving ( Functor, Applicative, Monad, MonadReader InterpreterEnv, MonadError PString )

instance MonadFail InterpreterM where
  fail s = throwError (PStr (s, Nothing))

data Result = Value Prim
            | Function (Result -> InterpreterM Result)
            | Instance (Map String Result)

instance Show Result where
  show (Value x) = show x
  show (Function _) = "<function>"
  show (Instance _) = "<instance>"

interpretExp :: InterpreterEnv -> Exp -> Either PString Result
interpretExp env =  flip runReader env . runExceptT . runMonad .cataRec alg
    where alg (Ann _ (Lit x)) = return $ Value x
          alg (Ann l (Var n)) =
            do ctx <- ask
               if not (member n ctx) then throwError (PStr (n, l))
               else return $ ctx!n
          alg (Ann _ (VarPat n)) = return $ Value (S n)
          alg (Ann _ (Lam e1 e2)) =
            do ctx <- ask
               (Value (S n))  <- e1
               return $ Function (\r -> local (\ctx' -> insert n r ctx' `union` ctx) e2)
          alg (Ann _ (Let e1 e2 e3)) =
            do (Value (S n)) <- e1
               v' <- e2
               local (insert n v') e3
          alg (Ann _ (App e1 e2)) =
            do (Function f) <- e1
               x <- e2
               f x
          alg (Ann _ (IfThenElse e1 e2 e3)) =
            do (Value (B b)) <- e1
               if b then e2 else e3
          alg _ = undefined


interpret :: InterpreterEnv -> Exp -> Either PString InterpreterEnv
interpret env e@(In (Ann _ (Let p _ _))) =
  (\v -> insert n v env) <$> interpretExp env e
  where (_, n) = extractNameFromPat p
interpret env e =
   (\v -> insert "it" v env) <$> interpretExp env e

interpretModule :: InterpreterEnv -> [Exp] -> Either PString InterpreterEnv
interpretModule = foldM interpret