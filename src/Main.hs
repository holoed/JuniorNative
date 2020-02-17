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
env = toEnv [("id", Set.fromList [] :=> TyLam (TyVar "a") (TyVar "a")),
            ("==", Set.fromList [IsIn "Eq" (TyVar "a")] :=> TyLam (TyVar "a") (TyLam (TyVar "a") (TyCon "Bool" []))),
            ("-",  Set.fromList [IsIn "Num" (TyVar "a")] :=> TyLam (TyVar "a") (TyLam (TyVar "a") (TyVar "a"))),
            ("+",  Set.fromList [IsIn "Num" (TyVar "a")] :=> TyLam (TyVar "a") (TyLam (TyVar "a") (TyVar "a"))),
            ("*",  Set.fromList [IsIn "Num" (TyVar "a")] :=> TyLam (TyVar "a") (TyLam (TyVar "a") (TyVar "a"))),
            ("/",  Set.fromList [IsIn "Fractional" (TyVar "a")] :=> TyLam (TyVar "a") (TyLam (TyVar "a") (TyVar "a"))),
            (">",  Set.fromList [IsIn "Ord" (TyVar "a")] :=> TyLam (TyVar "a") (TyLam (TyVar "a") (TyCon "Bool" []))),
            ("<",  Set.fromList [IsIn "Ord" (TyVar "a")] :=> TyLam (TyVar "a") (TyLam (TyVar "a") (TyCon "Bool" []))),
            ("fst", Set.fromList [] :=> TyLam (TyCon "Tuple" [TyVar "a", TyVar "b"]) (TyVar "a")),
            ("snd", Set.fromList [] :=> TyLam (TyCon "Tuple" [TyVar "a", TyVar "b"]) (TyVar "b")),
            ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a")] :=> TyLam (TyCon "Int" []) (TyVar "a")),
            ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a")] :=> TyLam (TyCon "Double" []) (TyVar "a"))
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
