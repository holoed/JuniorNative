{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans ()
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Map (Map, empty, fromList)
import Data.HashMap.Strict ((!))
import Monads ()
import Types ( Type(..), Qual((:=>)), Pred(IsIn) )
import Environment ( Env, toEnv )
import System.Console.Haskeline ()
import Modules (typeOfModule)
import Data.Maybe ( fromMaybe )
import Data.Monoid        ((<>))
import Data.Text.Lazy as Lazy ( pack )
import Data.Text as Text (pack, unpack, replace)
import System.Environment (lookupEnv)
import Web.Scotty         (ActionM, ScottyM, scotty)
import Web.Scotty.Trans ( body, jsonData, text, post, middleware )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Data.Aeson (FromJSON, ToJSON, Object, Value(String))
import Data.ByteString.Lazy.Char8 as Char8 ( unpack )

tyLam :: Type -> Type -> Type
tyLam t1 = TyApp (TyApp (TyCon "->") t1)

env :: Env
env = toEnv [("id", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
            ("==", Set.fromList [IsIn "Eq" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("-",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("+",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("*",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("/",  Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            (">",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("<",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("fst", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "a" 0)),
            ("snd", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "b" 0)),
            ("null", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Bool")),
            ("empty", Set.fromList [] :=> TyApp (TyCon "List") (TyVar "a" 0)),
            ("hd", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
            ("tl", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
            ("cons", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
            ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyCon "Int") (TyVar "a" 0)),
            ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyCon "Double") (TyVar "a" 0))]

classEnv :: [Qual Pred]
classEnv = [
  Set.fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Eq" (TyVar "b" 0)] :=> IsIn "Eq" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
  ]

main :: IO ()
main = do
  Prelude.putStrLn "Junior Service started"
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  let p = read pStr :: Int
  scotty p route

route :: ScottyM()
route = do
    middleware logStdoutDev
    post "/" $ do
         code <- body
         text $ Lazy.pack $ format (typeOfModule classEnv env (Char8.unpack code)) ++ "\r\n"
  where fromJson (String s) = Text.unpack (Text.replace "\\n" "\n" s)
        format = Data.List.intercalate "\n" . ((\(n, t) -> n ++ ": " ++ t) <$>)
