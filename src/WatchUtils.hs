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

-- TODO it would be awesome, for cross-platform purposes, to use a typeclass
-- here for subscription. Then, we could simly import kqueues or inotify (I'm
-- not looking to support windows, tbh).
-- We also need to massively restructure how this all works.
-- Basically, it's a lot clearer to me how this should be architected:
--   * A single watch mode subscribes to a single source.
--   * A 'triggers' mode which allows setting up multiple subscriptions.
--
--  Both modes can have recursive flags, allowing for subscriptions on subdirectories.

-- | Maximum size for bounded event channels.
maxChanSize :: Int
maxChanSize = 4096
-- TODO maybe change this to 1,
-- and force the thing below to call (blocking) callProcess
-- The problem is that we still have a system which creates
-- new threads for every entity we want watched.
-- It would be best if we had a single queue that's duped
-- across all the same entities and coherently shared.
-- Then, a size of 1 and `tryWriteChan` would designate true
-- load shedding and mean we have a better 'single threaded',
-- blocking model, which is ideal for testing and the like.

-- | Subscribe to particular event types on the given path
-- and write to a given channel
subscribe :: INotify -- ^ The notification object
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
{- runCmd cmd = callProcess cmd [] -- TODO To be used for single watch mode. -}

-- | Execute cmd on receival of any messages from the provided channel.
withEventChan :: OutChan Event -> IO a -> IO ()
withEventChan chan cmd = void . forkIO . forever $ readChan chan >> cmd

-- | Watch a directory and run a command all as concurrent actions.
-- returns the WatchDescriptor for later removal.
watchWith :: INotify -- ^ The notification object
          -> FilePath
          -> FilePath
          -> IO WatchDescriptor
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
