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
            ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> TyLam (TyCon "Int") (TyVar "a" 0)),
            ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> TyLam (TyCon "Double") (TyVar "a" 0))
     ]

process :: String -> IO ()
process input = do
  let ast = parseExpr input 
  let ty = ast >>= (infer env . liftN)
  putStrLn (either id pretty ast)
  putStrLn (either id (show . snd) ty)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Junior> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> liftIO $ process input
    loop
