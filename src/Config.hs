{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( parseConfig
  ) where

import           Control.Monad (mzero)
import           Data.Text     (Text)
import           Data.Yaml     (FromJSON, Value (Object), (.:))
import qualified Data.Yaml     as Y
import           GHC.Generics

data Config = Config { triggers :: [TriggerItem] } deriving (Show, Generic)

instance FromJSON Config

data TriggerItem = TriggerItem
  { _name    :: Text
  , _dirs    :: [FilePath]
  , _actions :: [Text]
  } deriving Show

instance FromJSON TriggerItem where
  parseJSON (Object v) = TriggerItem     <$>
                         v .: "name"    <*>
                         v .: "dirs"    <*>
                         v .: "actions"
  parseJSON _ = mzero

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig = Y.decodeFile
