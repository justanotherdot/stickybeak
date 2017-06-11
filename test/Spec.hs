module Main where

import           Stickybeak
import           Test.QuickCheck

prop_noResumeAfterExit :: Bool
prop_noResumeAfterExit =
    let rv = runPure sb
    in rv == Left "Exit"
  where
    sb = do
      args <- checkArgs
      job <- subscribe args
      unsubscribe job
      exit
      unsubscribe job

main :: IO ()
main = do
  quickCheck prop_noResumeAfterExit
