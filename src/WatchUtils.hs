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
                                                        createProcess, proc)

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
runCmd :: FilePath -> [FilePath] -> FilePath -> IO ()
runCmd cmd args path = void $ createProcess (proc cmd args){ cwd = Just path }

-- | Execute cmd on receival of any messages from the provided channel.
withEventChan :: OutChan Event -> IO a -> IO ()
withEventChan chan cmd = void . forkIO . forever $ readChan chan >> cmd

-- | Watch a directory and run a command all as concurrent actions.
-- returns the WatchDescriptor for later removal.
watchWith :: FilePath -> [FilePath] -> FilePath -> IO WatchDescriptor
watchWith cmd args dir = do
  (eventChan, wd) <- subscribe [Modify] dir
  withEventChan eventChan (runCmd cmd args dir)
  return wd

-- | Watch all non-hidden subdirectories in addition to dir.
watchWithRec :: FilePath -> [FilePath] -> FilePath -> IO [WatchDescriptor]
watchWithRec cmd args dir = do
    subDirs <- subDirectories dir
    mapM (watchWith cmd args) (filter (not . hidden) subDirs)
  where hidden path = head path == '.' && length path > 1

-- | Setup a trigger as specified in a TriggerItem
watchFromTrigger :: TriggerItem -> IO [WatchDescriptor]
watchFromTrigger TriggerItem{..} =
    if recursive
      then concat <$> mapM (watchWithRec cmd' cmdArgs') dirs
      else mapM (watchWith cmd' cmdArgs') dirs
  where (cmd', cmdArgs') = let cs = words cmd
                           in  (head cs, tail cs)

subDirectories :: FilePath -> IO [FilePath]
subDirectories dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      es <- fmap ((if dir == "." then "" else dir)</>) <$> listDirectory dir
      ds <- mapM subDirectories es
      return (dir : concat ds)
    else return []
