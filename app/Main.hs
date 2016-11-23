{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config             (Config (triggers), parseConfig)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Exit        (ExitCode (ExitFailure), exitWith)
import           WatchUtils         (setupTrigger)
-- import           OS                  (OS (..), detectOS)

import           Control.Concurrent
import           Control.Monad      (forever)
import           System.INotify

-- Below stipulates the basic logic for forking and subscribing to a basic
-- event channel. This way, we can subscribe to an event chan per directory.
main :: IO ()
main = do
  inotify <- initINotify
  eventChan <- newChan
  -- print inotify
  wd <- addWatch
          inotify
          [Open,Close,Access,Modify,Move]
          "."
          (writeChan eventChan)
  -- print wd
  putStrLn "Listens to your home directory. Hit enter to terminate."
  _ <- forkIO $ forever (readChan eventChan >>= \evt -> T.putStrLn (T.pack (show evt)))
  _ <- getLine
  removeWatch wd

testTriggers :: IO ()
testTriggers = do
  config <- parseConfig "stickybeak.yaml"
  case config of
    Just config' -> mapM_ setupTrigger (triggers config')
    Nothing      -> do
      T.putStrLn "Invalid config file provided"
      exitWith $ ExitFailure 1
