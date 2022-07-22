{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Junior.Utils.StringUtils (padR)
import Junior.Parser.Location (Loc(..), PString(..))
import Control.Monad.Trans ()
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List (intersperse)
import System.Console.Haskeline ()
import Data.Maybe ( fromMaybe )
import System.Environment (lookupEnv)
import Web.Scotty         (ScottyM, scotty)
import Web.Scotty.Trans ( body, json, post, get, middleware )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import Data.ByteString.Lazy.Char8 as Char8 ( unpack )
import Junior.Compiler.Intrinsics (classEnv )
import Data.Aeson
    ( ToJSON(toJSON), object, KeyValue((.=)) )
import Junior.Compiler.Compiler (fullInterp, backendPrinted, frontEndPrinted, fullJSClosedANF)
import Junior.Compiler.CompilerMonad (CompileM, run)
import qualified Junior.Compiler.SymbolTable as S
import qualified Junior.Interpreter.InterpreterIntrinsics as Interp (env)
import Data.Text (Text, pack)
import qualified Junior.TypeChecker.Environment as Environment
import Data.Bifunctor (second)
import Junior.Compiler.Junior (prelude)
import Junior.Pretty.PrettyTypes (prettyQ)

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

compile :: (String -> CompileM Text) -> String -> IO (Either PString (Text, [S.Symbol ]))
compile strategy code = do
   (_, preludeEnv) <- prelude
   let tableWidth = 49
   let line = replicate tableWidth '-'
   putStrLn ("+" ++ line ++ "+")
   putStrLn $ padR tableWidth "| Junior Compilation " ++ " |"
   putStrLn ("|" ++ line ++ "|")
   (x, (_, _, ss, _), z) <- run (strategy code) ("main", Interp.env) (classEnv, preludeEnv, [], [])
   mapM_ (\s -> putStrLn $ padR tableWidth ("| " ++ s) ++ " |") (intersperse (drop 2 line) z)
   putStrLn ("+" ++ line ++ "+")
   return $ (, ss) <$> x

route :: ScottyM()
route = do
    middleware logStdout
    post "/type" $ do
         code <- body
         ret <- liftIO $ compile frontEndPrinted (Char8.unpack code)
         either json json ret
    post "/compile" $ do
         code <- body
         ret <- liftIO $ compile backendPrinted (Char8.unpack code)
         either json json ret
    post "/run" $ do
         code <- body
         ret <- liftIO $ compile fullInterp (Char8.unpack code)
         either json json ret
    post "/compileToJs" $ do
         code <- body
         ret <- liftIO $ compile fullJSClosedANF (Char8.unpack code)
         either json json ret
    get "/libJs" $ do
         baseJs <- liftIO $ readFile "src/Junior/JavaScript/baseClosedLib.js"
         (preludeJs, _) <- liftIO $ prelude
         json (pack baseJs <> pack "\r\n\r\n" <> preludeJs)
    get "/libTypes" $ do
         (_, preludeEnv) <- liftIO $ prelude
         json $ second (show . prettyQ) <$> Environment.fromEnv preludeEnv

