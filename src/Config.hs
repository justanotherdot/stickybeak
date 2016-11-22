{-# LANGUAGE OverloadedStrings #-}

module Config
  ( parseConfig
  ) where

import qualified Data.Text.IO    as T
import           Text.Toml       (parseTomlDoc)
import           Text.Toml.Types (Table)


parseConfig :: FilePath -> IO Table
parseConfig path = do
  configText <- T.readFile path
  case parseTomlDoc "" configText of
    Right parsedConfig -> return parsedConfig
    Left  _            -> error "Error parsing config file"
