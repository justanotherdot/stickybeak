{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config       (parseConfig)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
-- import           OS                  (OS (..), detectOS)

main :: IO ()
main = do
  config <- parseConfig ".stickybeak.toml"
  T.putStrLn (T.pack (show config))
