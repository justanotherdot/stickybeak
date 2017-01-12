{-# LANGUAGE RecordWildCards #-}

module Main where

import           Config           (Config (..), defaultConfig, parseConfig)
import           Control.Monad    (unless, void, when)
import           Data.Maybe       (fromMaybe, isNothing)
import           Flags
import           System.Directory (doesFileExist)
import           System.Exit      (exitFailure)
import           System.INotify   (removeWatch)
import           System.IO        (hPutStrLn, stderr)
import           WatchUtils       (watch, watchFromTrigger)

main :: IO ()
main = stickybeak =<< getCmdLine

stickybeak :: SBMode -> IO ()
stickybeak mode =
  case mode of
    Watch{..}    -> watchMode dir cmd
    Triggers{..} -> triggerMode (fromMaybe defaultConfig config)

waitToQuit :: IO ()
waitToQuit = putStrLn "ctrl-c to quit" >> void getLine

watchMode :: Maybe FilePath -> Maybe FilePath -> IO ()
watchMode dir cmd = do
  cmd' <- case cmd of
            Nothing -> hPutStrLn stderr "Error: Did not provide a command to run" >> exitFailure
            Just c  -> return $ words c
  let dir' = fromMaybe "." dir
  wd <- watch dir' (head cmd') (tail cmd')
  waitToQuit
  removeWatch wd

triggerMode :: FilePath -> IO ()
triggerMode path = do
    fileExists <- doesFileExist path
    unless fileExists (hPutStrLn stderr ("Error: Could not find " ++ path) >> exitFailure)
    conf <- parseConfig path
    when (isNothing conf) (hPutStrLn stderr ("Error: Could not parse " ++ path) >> exitFailure)
    let Config{..} = fromMaybe Config{triggers = []} conf
    wds <- mapM watchFromTrigger triggers
    waitToQuit
    removeWatches wds
  where removeWatches = mapM_ (mapM_ removeWatch)
