{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module StickyBeak (stickybeak) where

import           Config           (Config (..), defaultConfig, parseConfig)
import           Control.Monad    (unless, void, when)
import           Data.Maybe       (fromMaybe, isNothing)
import           Error            (exitFailureMsg)
import           Flags
import           System.Directory (doesFileExist)
import           System.INotify   (removeWatch)
import           System.INotify
import           WatchUtils       (watchFromTrigger, watchWith, watchWithRec)

stickybeak :: IO ()
stickybeak = do
  inotify <- initINotify
  mode <- getCmdLine
  case mode of
    Watch{..}    -> watchMode inotify dir (unwords cmd) recursive
    Triggers{..} -> triggerMode inotify (fromMaybe defaultConfig config)

waitToQuit :: IO ()
waitToQuit = do
    putStrLn "hit enter to quit"
    void getLine

checkForCmd :: String -> IO String
checkForCmd cmd =
  case cmd of
    ""   -> exitFailureMsg "Error: Did not provide a command to run"
    cmd' -> return cmd'

watchMode :: INotify -> FilePath -> String -> Bool -> IO ()
watchMode inotify dir cmd rec = do
    cmd' <- checkForCmd cmd
    wds <- if rec
              then watchWithRec inotify cmd' dir
              else (:[]) <$> watchWith inotify cmd' dir
    waitToQuit
    removeWatches wds
  where removeWatches = mapM_ removeWatch

triggerMode :: INotify -> FilePath -> IO ()
triggerMode inotify path = do
    fileExists <- doesFileExist path
    unless fileExists (exitFailureMsg $ "Error: Could not find " ++ path)
    conf <- parseConfig path
    when (isNothing conf) (exitFailureMsg $ "Error: Could not parse " ++ path)
    let Config{..} = fromMaybe Config{triggers = []} conf
    wds <- concat <$> mapM (watchFromTrigger inotify) triggers
    waitToQuit
    removeWatches wds
  where removeWatches = mapM_ removeWatch
