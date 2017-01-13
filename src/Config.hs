{-# LANGUAGE OverloadedStrings #-}

module Config
  ( parseConfig
  , Config (..)
  , TriggerItem (..)
  , defaultConfig
  ) where

import           Control.Monad (mzero)
import           Data.Yaml     (FromJSON, Value (Object), (.!=), (.:), (.:?))
import qualified Data.Yaml     as Y

data Config = Config { triggers :: ![TriggerItem] } deriving Show

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .: "triggers"
  parseJSON _          = mzero

data TriggerItem = TriggerItem
  { dirs      :: ![FilePath]
  , cmd       :: !FilePath
  , args      :: ![FilePath]
  , recursive :: !Bool
  } deriving Show

instance FromJSON TriggerItem where
  parseJSON (Object v) = TriggerItem           <$>
                         v .:  "dirs"          <*>
                         v .:  "cmd"           <*>
                         v .:? "args" .!= [""] <*>
                         v .:? "recursive" .!= False
  parseJSON _          = mzero

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig = Y.decodeFile

defaultConfig :: String
defaultConfig = ".stickybeak.yml"
