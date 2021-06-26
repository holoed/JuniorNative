module Main where

import Control.Monad.Trans ( MonadIO(liftIO) )
import qualified Data.Set as Set
import Data.Map (Map, empty, fromList)
import Fixpoint ()
import RecursionSchemes ()
import Monads ()
import Ast ()
import Types ( Type(..), Qual((:=>)), Pred(IsIn) )
import Environment ( Env, toEnv )
import Infer (infer)
import System.Console.Haskeline ( defaultSettings, getInputLine, outputStrLn, runInputT )
import Parser (parseExpr)
import PrettyPrinter ( pretty )
import LiftNumbers ( liftN )
import SynExpToExp (toExp)
import Data.Functor ((<&>))
import Intrinsics ( env ) 

process :: String -> IO ()
process input = do
  let ast = parseExpr input
  let ty = ast >>= (infer [] env . liftN . toExp . head)
  putStrLn (either id pretty (ast <&> head))
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
