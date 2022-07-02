{-# LANGUAGE QuasiQuotes #-}
module UnitTests.MandelbrotSpec where

import Data.String.Interpolate ( i )
import Test.Sandwich (TopSpec, shouldBe, describe, it, parallel)
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Compiler ( fullInterp )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Cont (MonadIO(liftIO))

interpPrelude :: String
interpPrelude = [i|
val (.) :: (b -> c) -> (a -> b) -> a -> c
let (.) f g = \\x -> f (g x)
|]

build :: String -> IO String
build code = do
   (x, _, _) <- run (fullInterp (interpPrelude <> "\r\n\r\n" <> code)) ("main", Interp.env) (classEnv, env, [], [])
   return $ either show unpack x

(--->) :: (MonadIO m, MonadThrow m, MonadFail m) => FilePath -> String -> m () 
(--->) x y = do handle <- liftIO $ openFile x ReadMode
                contents <- liftIO $ hGetContents handle
                liftIO $ build contents >>= (`shouldBe` y)

tests :: TopSpec
tests = parallel $ do
  describe "Mandelbrot Tests" $ do

   it "Haskell vs Junior" $ 
     "tests/jnrs_lib/example2.jnr" ---> show (render mandelbrot 10)

range :: (Int -> a) -> Int -> Int -> [a]
range f startIndex endIndex =
 let range' acc endIndex' =
           if startIndex > endIndex' then acc
           else range' (f endIndex' : acc) (endIndex' - 1) in
       range' [] endIndex

partition :: Int -> [a] -> ([a], [a])
partition n xs =
   let partition' n' acc xs' =
        if n' == 0 || null xs' then (reverse acc, xs')
        else partition' (n' - 1) (head xs' : acc) (tail xs') in
    partition' n [] xs

split :: Int -> [a] -> [[a]]
split n xs =
  let split' acc xs' =
        if null xs' then acc
        else
        let pair = partition n xs' in
            split' (fst pair : acc) (snd pair) in
    reverse (split' [] xs)

posToCoord :: Int -> Int -> (Int, Int)
posToCoord sz x =
      let row = x `div` sz in
      let col = x - sz * row in
      (row, col)

norm :: (Double, Double) -> Double 
norm (re, im) = re * re + im * im

cadd :: (Double, Double) -> (Double, Double) -> (Double, Double)
cadd (re1, im1) (re2, im2) = (re1 + re2, im1 + im2)

cmul :: (Double, Double) -> (Double, Double) -> (Double, Double)
cmul (re1, im1) (re2, im2) = (re1 * re2 - im1 * im2, re1 * im2 + im1 * re2)

mPoint :: Int-> (Double , Double) -> (Double, Double) -> Int
mPoint i' c z =
  if i' == 65 || norm z > 4.0 then i' else mPoint (i' + 1) c (cadd (cmul z z) c)

toDouble :: Int -> Double
toDouble = fromIntegral  

mandelbrot :: Int -> (Int, Int) -> (Int, Int, Int)
mandelbrot s (x, y) =
  let x' = 4.0 * toDouble y / toDouble s - 2.5 in
  let y' = 4.0 * toDouble x / toDouble s - 2.0 in
  let i' = mPoint 0 (x', y') (0.0, 0.0) in
  let f i'' = 128 + truncate (128.0 * cos (toDouble i'' * 0.3)) in
  (f i', f (i' + 16), f (i' + 32))

render :: (Int -> (Int, Int) -> (Int, Int, Int)) -> Int -> [[(Int, Int, Int)]]
render effect pixSize =
  let f = effect pixSize . posToCoord pixSize in
  split pixSize (range f 0 (pixSize * pixSize))


