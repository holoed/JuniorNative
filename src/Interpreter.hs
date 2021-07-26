{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Interpreter where

import Fixpoint (Fix(In))
import Location ( PString(..) )
import Annotations ( Ann(..) )
import Ast (Exp, ExpF(..), extractNameFromPat)
import Control.Monad.Error.Class (MonadError(..))
import RecursionSchemes (cataRec)
import Data.HashMap ( Map, (!), insert, union, member )
import Control.Monad.Fail ()
import Control.Monad (foldM)
import Data.List (foldl')
import Data.Text (Text, pack, intercalate)
import qualified Primitives as P

type InterpreterEnv = Map Text Result

newtype InterpreterM a = InterpreterM { runMonad :: InterpreterEnv -> Either PString a }
  deriving ( Functor )

instance Applicative InterpreterM where
  {-# INLINE pure #-}
  pure x = InterpreterM (\_ -> Right x)
  {-# INLINE (<*>) #-}
  (<*>) mf mx = InterpreterM(\env -> case runMonad mf env of
                                      Right f -> case runMonad mx env of
                                                  Right x -> Right (f x)
                                                  Left s -> Left s
                                      Left s -> Left s)

instance Monad InterpreterM where
  {-# INLINE (>>=) #-}
  (>>=) m f = InterpreterM(\env -> case runMonad m env of
                                      Right x -> runMonad (f x) env
                                      Left s -> Left s)

instance MonadError PString InterpreterM where
  {-# INLINE throwError #-}
  throwError e = InterpreterM(\_ -> Left e)
  {-# INLINE catchError #-}
  catchError m f = InterpreterM(\env -> case runMonad m env of
                                        Right x -> Right x
                                        Left s -> runMonad (f s) env)

instance MonadFail InterpreterM where
  {-# INLINE fail #-}
  fail s = throwError (PStr (s, Nothing))

{-# INLINE ask #-}
ask :: InterpreterM InterpreterEnv
ask = InterpreterM Right

{-# INLINE local #-}
local :: (InterpreterEnv -> InterpreterEnv) -> InterpreterM a -> InterpreterM a
local f m = InterpreterM(runMonad m . f )

data Prim = U | I !Int | D !Double | B !Bool | S !Text deriving Eq

{-# INLINE toInterpPrim #-}
toInterpPrim :: P.Prim -> Prim
toInterpPrim P.U = U
toInterpPrim (P.I n) = I n
toInterpPrim (P.D x) = D x
toInterpPrim (P.B b) = B b
toInterpPrim (P.S s) = S (pack s)

{-# INLINE primToStr #-}
primToStr :: Prim -> Text
primToStr (I n) = pack (show n)
primToStr (D x) = pack (show x)
primToStr (B b) = pack (show b)
primToStr (S s) = s
primToStr U = "()"

data Result = Value !Prim
            | Function !(Result -> InterpreterM Result)
            | Instance !(Map String Result)
            | List ![Result]
            | Tuple ![Result]

{-# INLINE showResult #-}
showResult :: Result -> Text
showResult (Value x) = primToStr x
showResult (Function _) = "<function>"
showResult (Instance _) = "<instance>"
showResult (List xs) = "[" <> intercalate "," (showResult <$> xs) <> "]"
showResult (Tuple xs) = "(" <> intercalate "," (showResult <$> xs) <> ")"

{-# INLINE insertMany #-}
insertMany :: Result -> Result -> InterpreterEnv -> InterpreterEnv
insertMany (Value (S n)) v env = insert n v env
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
               local (insert n v') e3
          alg (Ann _ (App e1 e2)) =
            do (Function f) <- e1
               x <- e2
               f x
          alg (Ann _ (IfThenElse e1 e2 e3)) =
            do (Value (B b)) <- e1
               if b then e2 else e3

{-# INLINE interpret #-}
interpret :: InterpreterEnv -> Exp -> Either PString InterpreterEnv
interpret env e@(In (Ann _ (Let p _ _))) =
  (\v -> insert (pack n) v env) <$> interpretExp env e
  where (_, n) = extractNameFromPat p
interpret env e =
   (\v -> insert "it" v env) <$> interpretExp env e

interpretModule :: InterpreterEnv -> [Exp] -> Either PString InterpreterEnv
interpretModule = foldM interpret