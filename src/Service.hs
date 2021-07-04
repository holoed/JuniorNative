{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Location (Loc(..), PString(..))
import Control.Monad.Trans ()
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List (intercalate)
import System.Console.Haskeline ()
import Data.Maybe ( fromMaybe )
import Data.Text.Lazy as Lazy ( pack )
import System.Environment (lookupEnv)
import Web.Scotty         (ScottyM, scotty)
import Web.Scotty.Trans ( body, text, post, middleware )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Data.ByteString.Lazy.Char8 as Char8 ( unpack )
import Intrinsics ( env, classEnv ) 
import Data.Aeson
    ( ToJSON(toJSON), object, encode, KeyValue((.=)) ) 
import Compiler (pipeline)
import CompilerMonad (run)

instance ToJSON Loc where
  toJSON (Loc offset line column) = object ["len" .= offset, 
                                            "line"   .= line,
                                            "column" .= column]

instance ToJSON PString where
  toJSON (PStr (s, p)) = object ["msg" .= s,
                                 "loc" .= p]

main :: IO ()
main = do
  Prelude.putStrLn "Junior Service started"
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  let p = read pStr :: Int
  scotty p route

typeOfModule :: String -> IO (Either PString [(String, String)])
typeOfModule code = do 
   putStrLn "Start compilation"
   (x, _, z) <- run (pipeline code) (classEnv, env) []
   mapM_ putStrLn z
   return x

route :: ScottyM()
route = do
    middleware logStdoutDev
    post "/" $ do
         code <- body
         ret <- liftIO $ typeOfModule (Char8.unpack code)
         text $ Lazy.pack (either (Char8.unpack . encode) format ret ++ "\r\n")
  where format = Data.List.intercalate "\n" . ((\(n, t) -> n ++ ": " ++ t) <$>)
