{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Time.Clock
import Test.Sandwich

import qualified UnitTests

discoverDemo :: TopSpec
discoverDemo = describe "Discover" $ do
  UnitTests.tests

testOptions :: Options
testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions discoverDemo