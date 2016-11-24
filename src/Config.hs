{-# LANGUAGE OverloadedStrings #-}

module Config
  ( parseConfig
  , Config (..)
  , TriggerItem (..)
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mzero)
import           Data.Text           (Text)
import           Data.Yaml           (FromJSON, Value (Object), (.!=), (.:),
                                      (.:?))
import qualified Data.Yaml           as Y

data Config = Config { triggers :: ![TriggerItem] } deriving Show

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .: "triggers"
  parseJSON _          = mzero

data TriggerItem = TriggerItem
  { name :: !Text
  , dirs :: ![Text]
  , cmd  :: !Text
  , args :: ![Text]
  } deriving Show

instance FromJSON TriggerItem where
  parseJSON (Object v) = TriggerItem           <$>
                         v .:? "name" .!= ""   <*>
                         v .:  "dirs"          <*>
                         v .:  "cmd"           <*>
                         v .:? "args" .!= [""]
  parseJSON _          = mzero

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig = Y.decodeFile
