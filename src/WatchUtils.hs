{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module WatchUtils
  ( setupTrigger
  , subscribe
  ) where

import           Config               (TriggerItem (..))
import qualified Control.Exception    as E
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           System.Exit          (ExitCode (..))
import           System.Posix.Signals
import           System.Process       (CreateProcess (..), createProcess, proc)

import           Control.Concurrent
import           Control.Monad        (forever)
import           System.INotify

subscribe :: [EventVariety] -> FilePath -> IO (Chan Event, WatchDescriptor)
subscribe _eventTypes path = do
  inotify <- initINotify
  eventChan <- newChan
  -- print inotify
  wd <- addWatch
          inotify
          [Modify]
          path
          (writeChan eventChan)
  -- print wd
  putStrLn "Listens to your home directory. Hit enter to terminate."
  return (eventChan, wd)

-- TODO replace this ctrl-c business with daemonization.
-- | Handle SigInt from Ctrl-C on *nix systems to break out of the loop.
--   By default, loops at every second (1e6 microseconds)
handleCtrlC :: IO ()
handleCtrlC = do
  tid <- myThreadId
  _ <- installHandler keyboardSignal (Catch (E.throwTo tid ExitSuccess)) Nothing
  forever $ threadDelay 1000000

-- | Given a TriggerItem, setup a trigger provided it's specification.
setupTrigger :: TriggerItem -> IO ()
setupTrigger TriggerItem{..} = do
  runCmd (T.unpack cmd) (fmap T.unpack args) path
  handleCtrlC
  where path = T.unpack $ head dirs
  -- withManager $ \mgr -> do
  --   when (name /= "") $ T.putStrLn ("Setting up '" `append` name `append` "'")
  --   _stopSig <- watchDir mgr path (const True)

-- | Run a command on the current shell instance.
runCmd :: FilePath -> [FilePath] -> FilePath -> IO ()
runCmd cmd args dir = do
  _ <- createProcess (proc cmd args){ cwd = Just dir}
  return ()
