{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans
import qualified Data.Set as Set
import Data.Map (Map, empty, fromList)
import Fixpoint
import RecursionSchemes
import Monads
import Ast
import Types
import Environment
import Infer (infer)
import System.Console.Haskeline
import Parser (parseExpr)
import PrettyPrinter
import LiftNumbers
import SynExpToExp (toExp)
import Data.Maybe
import Data.Monoid        ((<>))
import Data.Text.Lazy     (Text)
import Data.Text.Lazy
import System.Environment (lookupEnv)
import Web.Scotty         (ActionM, ScottyM, scotty)
import Web.Scotty.Trans

env :: Env
env = toEnv [("id", Set.fromList [] :=> TyLam (TyVar "a" 0) (TyVar "a" 0)),
            ("==", Set.fromList [IsIn "Eq" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("-",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("+",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("*",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("/",  Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyVar "a" 0))),
            (">",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("<",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("fst", Set.fromList [] :=> TyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "a" 0)),
            ("snd", Set.fromList [] :=> TyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "b" 0)),
            ("isEmpty", Set.fromList [] :=> TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Bool")),
            ("empty", Set.fromList [] :=> TyApp (TyCon "List") (TyVar "a" 0)),
            ("hd", Set.fromList [] :=> TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
            ("tl", Set.fromList [] :=> TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
            ("cons", Set.fromList [] :=> TyLam (TyVar "a" 0) (TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
            ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> TyLam (TyCon "Int") (TyVar "a" 0)),
            ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> TyLam (TyCon "Double") (TyVar "a" 0))
     ]

process :: String -> String
process input = do
  let ast = parseExpr input 
  let ty = ast >>= (infer [] env . liftN . toExp . Prelude.head)
  either id (show . snd) ty

main :: IO ()
main = do
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  let p = read pStr :: Int
  scotty p route

route :: ScottyM()
route = post "/" $ do
          code <- param "code"
          text $ pack $ (process code) ++ "\r\n"