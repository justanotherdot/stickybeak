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
import           System.Process                        (createProcess, shell)

-- | Maximum size for bounded event channels.
maxChanSize :: Int
maxChanSize = 4096
-- TODO maybe change this to 1,
-- and force the thing below to call (blocking) callProcess

-- | Subscribe to particular event types on the given path
-- and write to a given channel
subscribe :: INotify -- ^ The notification object.
          -> [EventVariety] -- ^ The list of accepted event types
          -> FilePath -- ^ The patch to watch
          -> IO (OutChan Event, WatchDescriptor) -- ^ Event channel and watch descriptor used to unsubscribe.
subscribe inotify eventTypes path = do
  (inEventChan, outEventChan) <- newChan maxChanSize
  wd <- addWatch inotify eventTypes path (tryWriteChan inEventChan)
  putStrLn $ "Listening for changes on path '" ++ path ++ "'"
  return (outEventChan, wd)

-- | Run a command on the current shell instance.
-- always runs said command in the same directory that `stickybeak` was run.
runCmd :: FilePath -> IO ()
runCmd cmd = void $ createProcess $ shell cmd
{- runCmd cmd = callProcess cmd [] -} -- TODO To be used for single watch mode.

-- | Execute cmd on receival of any messages from the provided channel.
withEventChan :: OutChan Event -> IO a -> IO ()
withEventChan chan cmd = void . forkIO . forever $ readChan chan >> cmd

-- | Watch a directory and run a command all as concurrent actions.
-- returns the WatchDescriptor for later removal.
watchWith :: INotify -> FilePath -> FilePath -> IO WatchDescriptor
watchWith inotify cmd dir = do
  (eventChan, wd) <- subscribe inotify [CloseWrite] dir
  withEventChan eventChan $ runCmd cmd
  return wd

-- | Watch all non-hidden subdirectories in addition to dir.
watchWithRec :: INotify -> FilePath -> FilePath -> IO [WatchDescriptor]
watchWithRec inotify cmd dir = do
    subDirs <- subDirectories dir
    mapM (watchWith inotify cmd) (filter (not . hidden) subDirs)
  where hidden path = head path == '.' && length path > 1

-- | Setup a trigger as specified in a TriggerItem
watchFromTrigger :: INotify -> TriggerItem -> IO [WatchDescriptor]
watchFromTrigger inotify TriggerItem{..} =
  if recursive
     then concat <$> mapM (watchWithRec inotify cmd) dirs
     else mapM (watchWith inotify cmd) dirs

subDirectories :: FilePath -> IO [FilePath]
subDirectories dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      es <- fmap ((if dir == "." then "" else dir)</>) <$> listDirectory dir
      ds <- mapM subDirectories es
      return (dir : concat ds)
    else return []
