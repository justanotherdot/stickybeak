{-# LANGUAGE RecordWildCards #-}

module StickyBeak (stickybeak) where

import           Config           (Config (..), defaultConfig, parseConfig)
import           Control.Monad    (unless, void, when)
import           Data.Maybe       (fromMaybe, isNothing)
import           Error            (exitFailureMsg)
import           Flags
import           System.Directory (doesFileExist)
import           System.INotify   (removeWatch)
import           WatchUtils       (watchFromTrigger, watchWith, watchWithRec)

stickybeak :: IO ()
stickybeak = do
  mode <- getCmdLine
  case mode of
    Watch{..}    -> watchMode dir cmd recursive
    Triggers{..} -> triggerMode (fromMaybe defaultConfig config)

waitToQuit :: IO ()
waitToQuit = do
    putStrLn "hit enter to quit"
    void getLine

checkForCmd :: Maybe FilePath -> IO (FilePath, [FilePath])
checkForCmd cmd = do
  cmd' <- case cmd of
            Nothing -> exitFailureMsg "Error: Did not provide a command to run"
            Just c  -> return $ words c
  return (head cmd', tail cmd')

watchMode :: FilePath -> Maybe FilePath -> Bool -> IO ()
watchMode dir cmd rec = do
    (cmd', cmdArgs') <- checkForCmd cmd
    wds <- if rec
            then watchWithRec cmd' cmdArgs' dir
            else (:[]) <$> watchWith cmd' cmdArgs' dir
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
