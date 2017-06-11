module Main where

import           Stickybeak
import           Test.QuickCheck

prop_sanity :: Int -> Int -> Bool
prop_sanity n m = n + m == m + n

main :: IO ()
main = do
  quickCheck prop_sanity
