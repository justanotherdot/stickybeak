{-# LANGUAGE RecordWildCards #-}

module WatchUtils
  ( watchWith
  , watchFromTrigger
  , watchWithRec
  ) where

import           Config                                (TriggerItem (..))
import           Control.Concurrent                    (forkIO)
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad                         (forever, void)
import           System.Directory                      (doesDirectoryExist,
                                                        listDirectory)
import           System.FilePath                       ((</>))
import           System.INotify
import           System.Process                        (CreateProcess (..),
                                                        createProcess, shell)

-- | Maximum size for bounded event channels.
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
-- always runs said command in the same directory that `stickybeak` was run.
runCmd :: FilePath -> IO ()
runCmd cmd = void $ createProcess $ shell cmd

-- | Execute cmd on receival of any messages from the provided channel.
withEventChan :: OutChan Event -> IO a -> IO ()
withEventChan chan cmd = void . forkIO . forever $ readChan chan >> cmd

-- | Watch a directory and run a command all as concurrent actions.
-- returns the WatchDescriptor for later removal.
watchWith :: FilePath -> FilePath -> IO WatchDescriptor
watchWith cmd dir = do
  (eventChan, wd) <- subscribe [Modify] dir
  withEventChan eventChan $ runCmd cmd
  return wd

-- | Watch all non-hidden subdirectories in addition to dir.
watchWithRec :: FilePath -> FilePath -> IO [WatchDescriptor]
watchWithRec cmd dir = do
    subDirs <- subDirectories dir
    mapM (watchWith cmd) (filter (not . hidden) subDirs)
  where hidden path = head path == '.' && length path > 1

-- | Setup a trigger as specified in a TriggerItem
watchFromTrigger :: TriggerItem -> IO [WatchDescriptor]
watchFromTrigger TriggerItem{..} =
    if recursive
      then concat <$> mapM (watchWithRec cmd) dirs
      else mapM (watchWith cmd) dirs

subDirectories :: FilePath -> IO [FilePath]
subDirectories dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      es <- fmap ((if dir == "." then "" else dir)</>) <$> listDirectory dir
      ds <- mapM subDirectories es
      return (dir : concat ds)
    else return []
