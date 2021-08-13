{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import Fixpoint (Fix(In))
import Location ( PString(..) )
import Annotations ( Ann(..) )
import Ast (Exp, ExpF(..), extractNameFromPat)
import RecursionSchemes (cataRec)
import Control.Monad.Fail ()
import Control.Monad (foldM)
import Data.List (foldl')
import Data.Text ( pack  )
import Data.HashMap.Strict (empty)
import Control.Monad.Except (   MonadError(throwError) )
import InterpreterMonad ( InterpreterM(runMonad), ask, local, Result(Value, Tuple, Function), Prim(B, S), toInterpPrim, InterpreterEnv, member, (!), union, insertDynamic, insertStatic )

{-# INLINE insertMany #-}
insertMany :: Result -> Result -> InterpreterEnv -> InterpreterEnv
insertMany (Value (S n)) v env = insertDynamic n v env
insertMany (Tuple xs) (Tuple ys) env =
  foldl' (\acc (n, v) -> insertMany n v acc) env (zip xs ys)
insertMany _ _ _ = undefined

interpretExp :: InterpreterEnv -> Exp -> Either PString Result
interpretExp env e = runMonad (cataRec alg e) env
    where {-# INLINE alg #-}
          alg (Ann _ (Lit x)) = return $ Value (toInterpPrim x)
          alg (Ann _ (MkTuple xs)) = Tuple <$> sequence xs
          alg (Ann l (Var n)) =
            let n' = pack n in
            do ctx <- ask
               if not (member n' ctx) then throwError (PStr (n, l))
               else return $ ctx!n'
          alg (Ann _ (VarPat n)) = return $ Value (S $ pack n)
          alg (Ann _ (TuplePat ns)) = Tuple <$> sequence ns
          alg (Ann _ (Lam e1 e2)) =
            do ctx <- ask
               n  <- e1
               return $ Function (\v -> local (\ctx' -> insertMany n v (ctx `union` ctx')) e2)
          alg (Ann _ (Let e1 e2 e3)) =
            do (Value (S n)) <- e1
               v' <- e2
               local (insertDynamic n v') e3
          alg (Ann _ (App e1 e2)) =
            do (Function f) <- e1
               x <- e2
               f x
          alg (Ann _ (IfThenElse e1 e2 e3)) =
            do (Value (B b)) <- e1
               if b then e2 else e3
          alg (Ann _ (Defn _ _ e2)) = e2

{-# INLINE interpret #-}
interpret :: InterpreterEnv -> Exp -> Either PString InterpreterEnv
interpret (dict1, _) e@(In (Ann _ (Defn _ p _))) =
  (\v -> insertStatic (pack n) v (dict1, empty)) <$> interpretExp (dict1, empty) e
  where (_, n) = extractNameFromPat p
interpret (dict1, _) e =
   (\v -> insertStatic "it" v (dict1, empty)) <$> interpretExp (dict1, empty) e

interpretModule :: InterpreterEnv -> [Exp] -> Either PString InterpreterEnv
interpretModule = foldM interpret