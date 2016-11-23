{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config       (parseConfig, triggers)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           System.Exit  (ExitCode (ExitFailure), exitWith)
-- import           OS                  (OS (..), detectOS)

import           WatchUtils

main :: IO ()
main = setupTrigger "." print

spitOutTriggers :: IO ()
spitOutTriggers = do
  config <- parseConfig "stickybeak.yaml"
  let ts = triggers config
  case ts of
    Just _  -> T.putStrLn (T.pack $ show ts)
    Nothing -> do
      T.putStrLn "Invalid config file provided"
      exitWith $ ExitFailure 1
