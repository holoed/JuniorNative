module Main where

import Control.Monad.Trans ( MonadIO(liftIO) )
import qualified Data.Set as Set
import Data.Map (Map, empty, fromList)
import Fixpoint ( Fix(..) )
import RecursionSchemes ()
import Monads ()
import Ast ()
import Types ( Type(..), Qual((:=>)), Pred(IsIn) )
import Environment ( Env, toEnv )
import Infer (infer)
import System.Console.Haskeline ( defaultSettings, getInputLine, outputStrLn, runInputT )
import Parser (parseExpr)
import PrettyPrinter ( prettyPrint )
import PrettyTypes (prettyQ)
import LiftNumbers ( liftN )
import SynExpToExp (toExp)
import Data.Functor ((<&>))
import Intrinsics ( env, classEnv )
import Annotations (Ann(..))
import Data.Maybe (fromJust)

process :: String -> IO ()
process input = do
  let ast = parseExpr input
  let ty = ast >>= (infer classEnv env . liftN . (fromJust . toExp) . head)
  putStrLn (either show prettyPrint (ast <&> head))
  putStrLn (either show (show . (\(In (Ann (_, qt) _)) -> prettyQ qt) . snd) ty)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Junior> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> liftIO $ process input
    loop
