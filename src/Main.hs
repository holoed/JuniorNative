module Main where

import Control.Monad.Trans ( MonadIO(liftIO) )
import qualified Data.Set as Set
import Data.Map (Map, empty, fromList)
import Junior.Utils.Fixpoint ( Fix(..) )
import Junior.Utils.RecursionSchemes ()
import Junior.TypeChecker.Monads ()
import Junior.Core.Ast ()
import Junior.Core.Types ( Type(..), Qual((:=>)), Pred(IsIn) )
import Junior.TypeChecker.Environment ( Env, toEnv )
import Junior.TypeChecker.Infer (infer)
import System.Console.Haskeline ( defaultSettings, getInputLine, outputStrLn, runInputT )
import Junior.Parser.Parser (parseExpr)
import Junior.Pretty.Printer ( prettyPrint )
import Junior.Pretty.PrettyTypes (prettyQ)
import Junior.Compiler.LiftNumbers ( liftN )
import Junior.Parser.SynExpToExp (toExp)
import Data.Functor ((<&>))
import Junior.Compiler.Intrinsics ( env, classEnv )
import Junior.Utils.Annotations (Ann(..))
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
