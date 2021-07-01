{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans ()
import Data.List (intercalate)
import Monads ()
import System.Console.Haskeline ()
import Modules (typeOfModule)
import Data.Maybe ( fromMaybe )
import Data.Text.Lazy as Lazy ( pack )
import System.Environment (lookupEnv)
import Web.Scotty         (ScottyM, scotty)
import Web.Scotty.Trans ( body, text, post, middleware )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Data.ByteString.Lazy.Char8 as Char8 ( unpack )
import Intrinsics ( env, classEnv ) 

main :: IO ()
main = do
  Prelude.putStrLn "Junior Service started"
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  let p = read pStr :: Int
  scotty p route

route :: ScottyM()
route = do
    middleware logStdoutDev
    post "/" $ do
         code <- body
         text $ Lazy.pack $ either id format (typeOfModule classEnv env (Char8.unpack code)) ++ "\r\n"
  where format = Data.List.intercalate "\n" . ((\(n, t) -> n ++ ": " ++ t) <$>)
