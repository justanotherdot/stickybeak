module Main where

import           Control.Monad  (void)
import           System.INotify (EventVariety (..), removeWatch)
import           WatchUtils     (runCmd, subscribe, withEventChan)

main :: IO ()
main = do
  (eventChan, wd) <- subscribe [Modify] "src"
  withEventChan eventChan (runCmd "stack" ["test"] ".")
  putStrLn "Hit enter to quit"
  void getLine
  removeWatch wd
