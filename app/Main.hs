{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config       (parseConfig)
import           Data.Text    (pack)
import           Data.Text.IO
import           Prelude      hiding (putStrLn)
-- import           OS                  (OS (..), detectOS)

main :: IO ()
main = do
  config <- parseConfig ".stickybeak.toml"
  putStrLn (pack (show config))
