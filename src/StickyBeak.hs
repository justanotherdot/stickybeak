{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module StickyBeak (stickybeak) where

import           Config           (Config (..), defaultConfig, parseConfig)
import           Control.Monad    (unless, void, when)
import           Data.Maybe       (fromMaybe, isNothing)
import           Error            (exitFailureMsg)
import           Flags
import           System.Directory (doesFileExist)
import           System.INotify   (INotify, removeWatch, withINotify)
import           WatchUtils       (watchFromTrigger, watchWith, watchWithRec)

stickybeak :: IO ()
stickybeak = do
    mode <- getCmdLine
    case mode of
        Watch{..}    -> withINotify $ singleWatchMode dir (unwords cmd) recursive
        Triggers{..} -> withINotify $ triggerMode (fromMaybe defaultConfig config)

waitToQuit :: IO ()
waitToQuit = do
    putStrLn "hit enter to quit"
    void getLine -- TODO replace this with a proper ctrl-c handler.

checkForCmd :: String -> IO String
checkForCmd cmd = \case
    ""   -> exitFailureMsg "Error: Did not provide a command to run"
    cmd' -> return cmd'

singleWatchMode :: FilePath -> String -> Bool -> INotify -> IO ()
singleWatchMode dir cmd rec inotify = do
    cmd' <- checkForCmd cmd
    wds <- if rec
           then watchWithRec inotify cmd' dir
           else (:[]) <$> watchWith inotify cmd' dir
    waitToQuit
    removeWatches wds
  where removeWatches = mapM_ removeWatch

triggerMode :: FilePath -> INotify -> IO ()
triggerMode path inotify = do
    fileExists <- doesFileExist path
    unless fileExists (exitFailureMsg $ "Error: Could not find " ++ path)
    conf <- parseConfig path
    when (isNothing conf) (exitFailureMsg $ "Error: Could not parse " ++ path)
    let Config{..} = fromMaybe Config{triggers = []} conf
    wds <- concat <$> mapM (watchFromTrigger inotify) triggers
    waitToQuit
    removeWatches wds
  where removeWatches = mapM_ removeWatch
