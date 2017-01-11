{-# LANGUAGE DeriveDataTypeable #-}

module Flags (SBMode(..), getCmdLine) where

import           System.Console.CmdArgs

data SBMode = Watch { dir :: Maybe FilePath, cmd :: Maybe FilePath }
            | Triggers { config :: Maybe FilePath }
            deriving (Show, Data, Typeable)

watch :: SBMode
watch = Watch{ dir = def &= argPos 0 &= typ "DIR"
             , cmd = def &= argPos 1 &= typ "CMD"
             } &= help "Watch dir and run cmd on file changes"

triggers :: SBMode
triggers = Triggers{ config = def
                           &= help "Config file specifying triggers to setup"
                           &= typFile } &= help "Specify triggers from a config file"

cmdLineMode :: Mode (CmdArgs SBMode)
cmdLineMode = cmdArgsMode $ modes [triggers &= auto, watch]
               &= help "Watch directories and trigger tasks on changes"
               &= program "stickybeak"
               &= summary "stickybeak v0.1.0"

getCmdLine :: IO SBMode
getCmdLine = cmdArgsRun cmdLineMode
