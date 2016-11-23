{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config       (Config (triggers), parseConfig)
import qualified Data.Text.IO as T
import           System.Exit  (ExitCode (ExitFailure), exitWith)
import           WatchUtils   (setupTrigger)
-- import           OS                  (OS (..), detectOS)

main :: IO ()
main = testTriggers

testTriggers :: IO ()
testTriggers = do
  config <- parseConfig "stickybeak.yaml"
  case config of
    Just config' -> mapM_ setupTrigger (triggers config')
    Nothing      -> do
      T.putStrLn "Invalid config file provided"
      exitWith $ ExitFailure 1
