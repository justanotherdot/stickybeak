module WatchUtils
  ( subscribe
  , runCmd
  , withEventChan
  ) where

import           Control.Concurrent                    (forkIO)
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad                         (forever, void)
import           System.INotify
import           System.Process                        (CreateProcess (..),
                                                        createProcess, proc)

maxChanSize :: Int
maxChanSize = 4096

-- | Subscribe to events, as described by passed eventTypes, on path.
subscribe :: [EventVariety] -> FilePath -> IO (OutChan Event, WatchDescriptor)
subscribe eventTypes path = do
  inotify <- initINotify
  (inEventChan, outEventChan) <- newChan maxChanSize
  wd <- addWatch inotify eventTypes path (writeChan inEventChan)
  putStrLn $ "Listening for changes on path '" ++ path ++ "'"
  return (outEventChan, wd)

-- | Run a command on the current shell instance.
runCmd :: FilePath -> [FilePath] -> FilePath -> IO ()
runCmd cmd args cwd' = void $ createProcess (proc cmd args){ cwd = Just cwd' }

-- | Execute cmd on receival of any messages from the provided channel.
withEventChan :: OutChan Event -> IO a -> IO ()
withEventChan chan cmd = void . forkIO . forever $ readChan chan >> cmd
