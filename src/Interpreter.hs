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
import Data.HashMap.Strict ( HashMap, (!), insert, union, member )
import Primitives ( Prim(..), primToStr )
import Control.Monad.Fail ()
import Control.Monad (foldM)
import Data.List (intercalate, foldl')

type InterpreterEnv = HashMap String Result
newtype InterpreterM a = InterpreterM { runMonad :: ExceptT PString (Reader InterpreterEnv) a }
   deriving ( Functor, Applicative, Monad, MonadReader InterpreterEnv, MonadError PString )

instance MonadFail InterpreterM where
  fail s = throwError (PStr (s, Nothing))

data Result = Value Prim
            | Function (Result -> InterpreterM Result)
            | Instance (HashMap String Result)
            | List [Result]
            | Tuple [Result]

instance Show Result where
  show (Value x) = primToStr x
  show (Function _) = "<function>"
  show (Instance _) = "<instance>"
  show (List xs) = show xs
  show (Tuple xs) = "(" ++ intercalate "," (show <$> xs) ++ ")"

insertMany :: Result -> Result -> InterpreterEnv -> InterpreterEnv
insertMany (Value (S n)) v env = insert n v env
insertMany (Tuple xs) (Tuple ys) env = 
  foldl' (\acc (n, v) -> insertMany n v acc) env (zip xs ys) 
insertMany _ _ _ = undefined  

interpretExp :: InterpreterEnv -> Exp -> Either PString Result
interpretExp env =  flip runReader env . runExceptT . runMonad .cataRec alg
    where alg (Ann _ (Lit x)) = return $ Value x
          alg (Ann _ (MkTuple xs)) = Tuple <$> sequence xs
          alg (Ann l (Var n)) =
            do ctx <- ask
               if not (member n ctx) then throwError (PStr (n, l))
               else return $ ctx!n
          alg (Ann _ (VarPat n)) = return $ Value (S n)
          alg (Ann _ (TuplePat ns)) = Tuple <$> sequence ns
          alg (Ann _ (Lam e1 e2)) =
            do ctx <- ask
               n  <- e1
               return $ Function (\v -> local (\ctx' -> insertMany n v (ctx `union` ctx')) e2)
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

interpret :: InterpreterEnv -> Exp -> Either PString InterpreterEnv
interpret env e@(In (Ann _ (Let p _ _))) =
  (\v -> insert n v env) <$> interpretExp env e
  where (_, n) = extractNameFromPat p
interpret env e =
   (\v -> insert "it" v env) <$> interpretExp env e

interpretModule :: InterpreterEnv -> [Exp] -> Either PString InterpreterEnv
interpretModule = foldM interpret