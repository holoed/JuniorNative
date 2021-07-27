{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import StringUtils (padR)
import Location (Loc(..), PString(..))
import Control.Monad.Trans ()
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List (intersperse)
import System.Console.Haskeline ()
import Data.Maybe ( fromMaybe )
import Data.Text.Lazy as Lazy ( pack )
import System.Environment (lookupEnv)
import Web.Scotty         (ScottyM, scotty)
import Web.Scotty.Trans ( body, text, post, middleware )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import Data.ByteString.Lazy.Char8 as Char8 ( unpack )
import Intrinsics ( env, classEnv )
import Data.Aeson
    ( ToJSON(toJSON), object, encode, KeyValue((.=)) )
import Compiler (full, backendPrinted, frontEndPrinted)
import CompilerMonad (CompileM, run)
import qualified SymbolTable as S
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (Text, unpack)

instance ToJSON Loc where
  toJSON (Loc offset line column) = object ["len" .= offset,
                                            "line"   .= line,
                                            "column" .= column]

instance ToJSON PString where
  toJSON (PStr (s, p)) = object ["txt" .= s,
                                 "loc" .= p]

instance ToJSON S.Symbol where
  toJSON s = object ["name" .= S.name s,
                     "ty" .= show (S.ty s),
                     "parent" .= S.parent s,
                     "top" .= S.top s]

main :: IO ()
main = do
  Prelude.putStrLn "Junior Service started"
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  let p = read pStr :: Int
  scotty p route

compile :: (String -> CompileM Text) -> String -> IO (Either PString (String, [S.Symbol ]))
compile strategy code = do
   let tableWidth = 49
   let line = replicate tableWidth '-'
   putStrLn ("+" ++ line ++ "+")
   putStrLn $ padR tableWidth "| Junior Compilation " ++ " |"
   putStrLn ("|" ++ line ++ "|")
   (x, (_, ss), z) <- run (strategy code) (Interp.env, classEnv ) (env, [])
   mapM_ (\s -> putStrLn $ padR tableWidth ("| " ++ s) ++ " |") (intersperse (drop 2 line) z)
   putStrLn ("+" ++ line ++ "+")
   return $ (, ss) . Data.Text.unpack <$> x

route :: ScottyM()
route = do
    middleware logStdout
    post "/type" $ do
         code <- body
         ret <- liftIO $ compile frontEndPrinted (Char8.unpack code)
         text $ Lazy.pack (either (Char8.unpack . encode) (Char8.unpack . encode) ret)
    post "/compile" $ do
         code <- body
         ret <- liftIO $ compile backendPrinted (Char8.unpack code)
         text $ Lazy.pack (either (Char8.unpack . encode) (Char8.unpack . encode) ret)
    post "/run" $ do
         code <- body
         ret <- liftIO $ compile full (Char8.unpack code)
         text $ Lazy.pack (either (Char8.unpack . encode) (Char8.unpack . encode) ret)

