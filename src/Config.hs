{-# LANGUAGE OverloadedStrings #-}

module Config
  ( parseConfig
  , triggers
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mzero)
import           Data.Text           (Text)
import           Data.Yaml           (FromJSON, Value (Object), (.:))
import qualified Data.Yaml           as Y

data Config = Config { _triggers :: ![TriggerItem] } deriving Show

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .: "triggers"
  parseJSON _          = mzero

data TriggerItem = TriggerItem
  { _name    :: !Text
  , _dirs    :: ![FilePath]
  , _actions :: ![Text]
  } deriving Show

instance FromJSON TriggerItem where
  parseJSON (Object v) = TriggerItem    <$>
                         v .: "name"    <*>
                         v .: "dirs"    <*>
                         v .: "actions"
  parseJSON _          = mzero

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig = Y.decodeFile

triggers :: Maybe Config -> Maybe [TriggerItem]
triggers = fmap _triggers
