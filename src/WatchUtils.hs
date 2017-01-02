module WatchUtils
  ( subscribe
  , runCmd
  , withEventChan
  ) where

import           Control.Concurrent
import           Control.Monad      (forever, void)
import           System.INotify
import           System.Process     (CreateProcess (..), createProcess, proc)

-- | Subscribe to events, as described by passed eventTypes, on path.
subscribe :: [EventVariety] -> FilePath -> IO (Chan Event, WatchDescriptor)
subscribe eventTypes path = do
  inotify <- initINotify
  eventChan <- newChan
  wd <- addWatch
          inotify
          eventTypes
          path
          (writeChan eventChan)
  putStrLn "Listens to your home directory. Hit enter to terminate."
  return (eventChan, wd)

-- | Run a command on the current shell instance.
runCmd :: FilePath -> [FilePath] -> FilePath -> IO ()
runCmd cmd args dir = do
  void $ createProcess (proc cmd args){ cwd = Just dir}
  return ()

withEventChan :: Chan Event -> IO a -> IO ()
withEventChan chan cmd = void . forkIO . forever $ readChan chan >> cmd
