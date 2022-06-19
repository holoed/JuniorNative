{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Junior where

import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Compiler ( fullJSClosedANF )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text ( pack, unpack, Text )
import Location (PString)
import Environment (Env)
import Control.Monad ( foldM, (>=>) )
import System.FilePath (takeBaseName) 

build :: Either PString (Text, Env) -> (String, Text) -> IO (Either PString (Text, Env))
build (Right (js, env1)) (ns, code) = do
   (x, (_, env2, _, _), _) <- run (fullJSClosedANF (unpack code)) (ns, Interp.env) (classEnv, env1, [], [])
   return $ (\js2 -> (js <> pack "\r\n" <> js2, env2)) <$> x
build (Left x) _ = return $ Left x

buildAll :: (Text, Env) -> [(String, Text)] -> IO (Either PString (Text, Env))
buildAll base = foldM build (Right base)

fromFiles :: [FilePath] -> IO [(String, Text)]
fromFiles = mapM (\x-> do h <- openFile x ReadMode 
                          contents <- hGetContents h
                          return (takeBaseName x, pack contents))
               

buildAllFiles :: [FilePath] -> IO (Either PString (Text, Env))
buildAllFiles =  fromFiles >=> buildAll ("", env)  

prelude :: IO (Text, Env)
prelude = do x <- buildAllFiles ["src/prelude/prelude.jnr"]
             case x of 
                Right v -> return v
                Left err -> error (show err)