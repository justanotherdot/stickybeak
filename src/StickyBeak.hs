{-# LANGUAGE RecordWildCards #-}

module StickyBeak (stickybeak, subDirectories) where

import           Config           (Config (..), defaultConfig, parseConfig)
import           Control.Monad    (unless, void, when)
import           Data.Maybe       (fromMaybe, isNothing)
import           Error            (exitFailureMsg)
import           Flags
import           System.Directory (doesDirectoryExist, doesFileExist,
                                   listDirectory)
import           System.FilePath  (takeBaseName, (</>))
import           System.INotify   (removeWatch)
import           WatchUtils       (watch, watchFromTrigger)

stickybeak :: IO ()
stickybeak = do
  mode <- getCmdLine
  case mode of
    Watch{..}    -> watchMode dir cmd
    Triggers{..} -> triggerMode (fromMaybe defaultConfig config)

waitToQuit :: IO ()
waitToQuit = putStrLn "ctrl-c to quit" >> void getLine

checkWatchModeArgs :: Maybe FilePath -> Maybe FilePath -> IO (FilePath, String, [String])
checkWatchModeArgs dir cmd = do
  cmd' <- case cmd of
            Nothing -> exitFailureMsg "Error: Did not provide a command to run"
            Just c  -> return $ words c
  let dir' = fromMaybe "." dir
  return (dir', head cmd', tail cmd')

watchMode :: Maybe FilePath -> Maybe FilePath -> IO ()
watchMode dir cmd = do
  (dir', cmd', args) <- checkWatchModeArgs dir cmd
  wd <- watch dir' cmd' args
  waitToQuit
  removeWatch wd

subDirectories :: FilePath -> IO [FilePath]
subDirectories dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      es <- fmap (dir</>) <$> listDirectory dir
      ds <- mapM subDirectories es
      return (takeBaseName dir : concat ds)
    else return []

watchModeRec :: Maybe FilePath -> Maybe FilePath -> IO ()
watchModeRec dir cmd = do
    (dir', cmd', args) <- checkWatchModeArgs dir cmd
    subDirs <- subDirectories dir'
    wds <- traverse (\d -> watch d cmd' args) subDirs
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
    wds <- mapM watchFromTrigger triggers
    waitToQuit
    removeWatches wds
  where removeWatches = mapM_ (mapM_ removeWatch)
