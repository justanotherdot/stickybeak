{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           Config             (Config (triggers), parseConfig)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
-- import           System.Exit        (ExitCode (ExitFailure), exitWith)
import           WatchUtils         (subscribe)

import           Control.Concurrent
import           Control.Monad      (forever)
import           System.INotify

main :: IO ()
main = do
  (eventChan, wd) <- subscribe [Open] "src"
  _ <- forkIO $ forever (readChan eventChan >>= \evt -> T.putStrLn (T.pack (show evt)))
  _ <- getLine
  removeWatch wd

-- testTriggers :: IO ()
-- testTriggers = do
--   config <- parseConfig "stickybeak.yaml"
  -- case config of
  --   Just config' -> mapM_ setupTrigger (triggers config')
  --   Nothing      -> do
  --     T.putStrLn "Invalid config file provided"
  --     exitWith $ ExitFailure 1
