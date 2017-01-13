{-# LANGUAGE RecordWildCards #-}

module StickyBeak (stickybeak) where

import           Config           (Config (..), defaultConfig, parseConfig)
import           Control.Monad    (unless, void, when)
import           Data.Maybe       (fromMaybe, isNothing)
import           Error            (exitFailureMsg)
import           Flags
import           System.Directory (doesFileExist)
import           System.INotify   (removeWatch)
import           WatchUtils       (watch, watchFromTrigger, watchRec)

stickybeak :: IO ()
stickybeak = do
  mode <- getCmdLine
  case mode of
    Watch{..}    -> if recursive
                      then watchModeRec dir cmd
                      else watchMode dir cmd
    Triggers{..} -> triggerMode (fromMaybe defaultConfig config)

waitToQuit :: IO ()
waitToQuit = putStrLn "ctrl-c to quit" >> void getLine

checkWatchModeArgs :: Maybe FilePath
                   -> Maybe FilePath
                   -> IO (FilePath, String, [String])
checkWatchModeArgs dir cmd = do
  cmd' <- case cmd of
            Nothing -> exitFailureMsg "Error: Did not provide a command to run"
            Just c  -> return $ words c
  let dir' = fromMaybe "." dir
  return (dir', head cmd', tail cmd')

watchMode :: Maybe FilePath -> Maybe FilePath -> IO ()
watchMode dir cmd = do
  (dir', cmd', cmdArgs') <- checkWatchModeArgs dir cmd
  wd <- watch dir' cmd' cmdArgs'
  waitToQuit
  removeWatch wd

watchModeRec :: Maybe FilePath -> Maybe FilePath -> IO ()
watchModeRec dir cmd = do
    (dir', cmd', cmdArgs') <- checkWatchModeArgs dir cmd
    wds <- watchRec dir' cmd' cmdArgs'
    waitToQuit
    removeWatches wds
  where removeWatches = mapM_ removeWatch

triggerMode :: FilePath -> IO ()
triggerMode path = do
    fileExists <- doesFileExist path
    unless fileExists (exitFailureMsg $ "Error: Could not find " ++ path)
    conf <- parseConfig path
    when (isNothing conf) (exitFailureMsg $ "Error: Could not parse " ++ path)
    let Config{..} = fromMaybe Config{triggers = []} conf
    wds <- concat <$> mapM watchFromTrigger triggers
    waitToQuit
    removeWatches wds
  where removeWatches = mapM_ removeWatch
