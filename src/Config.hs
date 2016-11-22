{-# LANGUAGE OverloadedStrings #-}

module Config
  ( parseConfig
  ) where

import           Data.Text.IO    (readFile)
import           Prelude         hiding (readFile)
import           Text.Toml       (parseTomlDoc)
import           Text.Toml.Types (Table, emptyTable)


parseConfig :: FilePath -> IO Table
parseConfig path = do
  configText <- readFile path
  case parseTomlDoc "" configText of
    Right parsedConfig -> return parsedConfig
    Left  _            -> error "Error parsing config file"
