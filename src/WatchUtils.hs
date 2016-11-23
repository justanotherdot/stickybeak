{-# LANGUAGE OverloadedStrings #-}

module WatchUtils
  ( setupTrigger
  ) where

import           System.FSNotify

import           Control.Concurrent   (myThreadId, threadDelay)
import           System.Exit
import           System.Posix.Signals

import qualified Control.Exception    as E
import           Control.Monad        (forever)

handleCtrlC :: IO ()
handleCtrlC = do
  tid <- myThreadId
  _ <- installHandler keyboardSignal (Catch (E.throwTo tid ExitSuccess)) Nothing
  forever $ threadDelay 1000000

setupTrigger :: FilePath -> Action -> IO ()
setupTrigger path action =
  withManager $ \mgr -> do
    _stopSig <- watchDir mgr path (const True) action
    handleCtrlC
