{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module InterpreterMonad where

import Data.Text (Text, pack, intercalate)
import qualified Primitives as P
import Location ( PString(..) )
import Control.Monad.Error.Class ( MonadError(..) )
import qualified Data.HashMap.Strict as Map ( HashMap, (!), insert, union, member, lookup, empty )

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
            | Instance !(Map.HashMap String Result)
            | List ![Result]
            | Tuple ![Result]

{-# INLINE showResult #-}
showResult :: Result -> Text
showResult (Value x) = primToStr x
showResult (Function _) = "<function>"
showResult (Instance _) = "<instance>"
showResult (List xs) = "[" <> intercalate "," (showResult <$> xs) <> "]"
showResult (Tuple xs) = "(" <> intercalate "," (showResult <$> xs) <> ")"

type InterpreterEnv = (Map.HashMap Text Result, Map.HashMap Text Result)

insertStatic :: Text -> Result -> InterpreterEnv -> InterpreterEnv
insertStatic n v (dict1, dict2) = (Map.insert n v dict1, dict2)

insertDynamic :: Text -> Result -> InterpreterEnv -> InterpreterEnv
insertDynamic n v (dict1, dict2) = (dict1, Map.insert n v dict2)

member :: Text -> InterpreterEnv -> Bool
member n (dict1, dict2) =
    Map.member n dict1 || Map.member n dict2

(!) :: InterpreterEnv -> Text -> Result
(!) (dict1, dict2) n
  | Map.member n dict2 = (Map.!) dict2 n
  | Map.member n dict1 = (Map.!) dict1 n
  | otherwise = error "not found"

lookup :: Text -> InterpreterEnv -> Maybe Result
lookup n (dict1, dict2)
  | Map.member n dict2 = Map.lookup n dict2
  | Map.member n dict1 = Map.lookup n dict1
  | otherwise = Nothing 

union :: InterpreterEnv -> InterpreterEnv -> InterpreterEnv
union (dictA1, dictB1) (_, dictB2) = (dictA1, dictB1 `Map.union` dictB2)

empty :: InterpreterEnv
empty = (Map.empty , Map.empty )