{-# LANGUAGE OverloadedStrings #-}

module Config
  ( parseConfig
  ) where

import           Control.Monad (mzero)
import           Data.Text     (Text)
import           Data.Yaml     (FromJSON, Value (Object), (.:))
import qualified Data.Yaml     as Y

data ConfigItem = ConfigItem
  { _name    :: Text
  , _dirs    :: [FilePath]
  , _actions :: [Text]
  } deriving Show

instance FromJSON ConfigItem where
  parseJSON (Object v) = ConfigItem     <$>
                         v .: "name"    <*>
                         v .: "dirs"    <*>
                         v .: "actions"
  parseJSON _ = mzero

parseConfig :: FilePath -> IO (Maybe ConfigItem)
parseConfig = Y.decodeFile
