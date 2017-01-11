{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad          (void)
import           Data.Maybe             (fromMaybe)
import           Flags
import           System.Console.CmdArgs
import           System.INotify         (EventVariety (..), removeWatch)
import           WatchUtils             (runCmd, subscribe, withEventChan)

watchMode :: Maybe FilePath -> Maybe FilePath -> IO ()
watchMode dir cmd = do
  let cmd' = words (fromMaybe "" cmd)
  let dir' = fromMaybe "." dir
  (eventChan, wd) <- subscribe [Modify] dir'
  withEventChan eventChan (runCmd (head cmd') (tail cmd') dir')
  putStrLn "ctrl-c to quit"
  void getLine
  removeWatch wd

main :: IO ()
main = do
  x <- cmdArgsRun cmdLineMode
  case x of
    Watch{..}    -> watchMode dir cmd
    Triggers{..} -> undefined
