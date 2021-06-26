{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans ()
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Map (Map, empty, fromList)
import Data.HashMap.Strict ((!))
import Monads ()
import Types ( Type(..), Qual((:=>)), Pred(IsIn) )
import Environment ( Env, toEnv )
import System.Console.Haskeline ()
import Modules (typeOfModule)
import Data.Maybe ( fromMaybe )
import Data.Monoid        ((<>))
import Data.Text.Lazy as Lazy ( pack )
import Data.Text as Text (pack, unpack, replace)
import System.Environment (lookupEnv)
import Web.Scotty         (ActionM, ScottyM, scotty, addHeader)
import Web.Scotty.Trans ( body, jsonData, text, post, middleware )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Data.Aeson (FromJSON, ToJSON, Object, Value(String))
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
         addHeader "Access-Control-Allow-Origin" "*"
         text $ Lazy.pack $ format (typeOfModule classEnv env (Char8.unpack code)) ++ "\r\n"
  where fromJson (String s) = Text.unpack (Text.replace "\\n" "\n" s)
        format = Data.List.intercalate "\n" . ((\(n, t) -> n ++ ": " ++ t) <$>)
