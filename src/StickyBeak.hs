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
import           WatchUtils       (watchFromTrigger, watchWith, watchWithRec)

stickybeak :: IO ()
stickybeak = do
  mode <- getCmdLine
  case mode of
    Watch{..}    -> do
      cmd' <- checkForCmd (Just cmd)
      print cmd'
      watchMode dir cmd' recursive
    Triggers{..} -> triggerMode (fromMaybe defaultConfig config)

waitToQuit :: IO ()
waitToQuit = do
    putStrLn "hit enter to quit"
    void getLine

checkForCmd :: Maybe [FilePath] -> IO FilePath
checkForCmd cmd = do
  case cmd of
    Nothing   -> exitFailureMsg "Error: Did not provide a command to run"
    Just cmd' -> return $ unwords cmd'

watchMode :: FilePath -> FilePath -> Bool -> IO ()
watchMode dir cmd rec = do
    wds <- if rec
            then watchWithRec cmd dir
            else (:[]) <$> watchWith cmd dir
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
