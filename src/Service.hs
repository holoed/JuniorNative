{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Location (Loc(..), PString(..))
import Control.Monad.Trans ()
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List (intercalate)
import Monads (run)
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

typeOfModule :: String -> Either PString ([(String, String)], [String])
typeOfModule code = do 
   (x, _, z) <- run (pipeline code) (classEnv, env) []
   return (x, z)

route :: ScottyM()
route = do
    middleware logStdoutDev
    post "/" $ do
         code <- body
         let ret = typeOfModule (Char8.unpack code)
         x <- case ret of 
               Left err -> return $ (Char8.unpack . encode) err
               Right (v, w) -> do liftIO $ mapM_ print w
                                  return $ format v
         text $ Lazy.pack (x ++ "\r\n")
  where format = Data.List.intercalate "\n" . ((\(n, t) -> n ++ ": " ++ t) <$>)
