{-# LANGUAGE OverloadedStrings #-}
module Main where

import StringUtils (padR)
import Location (Loc(..), PString(..), getName)
import Control.Monad.Trans ()
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List (nub, intercalate, intersperse)
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
import qualified SymbolTable as S

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

extractNames :: [S.Symbol] -> [(String, String)]
extractNames ss = (\s -> (getName $ S.name s, show $ S.ty s)) <$> filter S.top ss

typeOfModule :: String -> IO (Either PString [(String, String)])
typeOfModule code = do
   let tableWidth = 49
   let line = replicate tableWidth '-'
   putStrLn ("+" ++ line ++ "+")
   putStrLn $ padR tableWidth "| Junior Compilation " ++ " |"
   putStrLn ("|" ++ line ++ "|")
   (x, _, z) <- run (pipeline code) classEnv env
   mapM_ (\s -> putStrLn $ padR tableWidth ("| " ++ s) ++ " |") (intersperse (drop 2 line) z)
   putStrLn ("+" ++ line ++ "+")
   return $ nub . extractNames . snd  <$> x

route :: ScottyM()
route = do
    middleware logStdoutDev
    post "/" $ do
         code <- body
         ret <- liftIO $ typeOfModule (Char8.unpack code)
         text $ Lazy.pack (either (Char8.unpack . encode) format ret ++ "\r\n")
  where format = Data.List.intercalate "\n" . ((\(n, t) -> n ++ ": " ++ t) <$>)
