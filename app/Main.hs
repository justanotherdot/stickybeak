{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config       (parseConfig)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           System.Exit  (ExitCode (ExitFailure), exitWith)
-- import           OS                  (OS (..), detectOS)

main :: IO ()
main = do
  config <- parseConfig "stickybeak.yaml"
  case config of
    Just x  -> T.putStrLn (T.pack $ show x)
    Nothing -> do
      T.putStrLn "Could not parse config file"
      exitWith $ ExitFailure 1
