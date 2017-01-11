{-# LANGUAGE RecordWildCards #-}

module Main where

import           Config                 (Config (..), parseConfig)
import           Control.Monad          (void)
import           Data.Maybe             (fromMaybe)
import           Flags
import           System.Console.CmdArgs
import           System.INotify         (removeWatch)
import           WatchUtils             (watch, watchFromTrigger)

watchMode :: Maybe FilePath -> Maybe FilePath -> IO ()
watchMode dir cmd = do
  let cmd' = words (fromMaybe "" cmd)
  let dir' = fromMaybe "." dir
  wd <- watch dir' (head cmd') (tail cmd')
  putStrLn "ctrl-c to quit"
  void getLine
  removeWatch wd

triggerMode :: FilePath -> IO ()
triggerMode path = do
  conf <- parseConfig path
  let Config{..} = fromMaybe Config{triggers = []} conf
  wds <- mapM watchFromTrigger triggers
  putStrLn "ctrl-c to quit"
  void getLine
  mapM_ (mapM_ removeWatch) wds

main :: IO ()
main = do
  x <- cmdArgsRun cmdLineMode
  case x of
    Watch{..}    -> watchMode dir cmd
    Triggers{..} -> case config of
                      Just path -> triggerMode path
                      Nothing   -> triggerMode "stickybeak.yaml"
