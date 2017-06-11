{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RecordWildCards           #-}

module Stickybeak
  ( stickybeak
  ) where

import           Control.Concurrent                    (ThreadId, forkIO)
import           Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan,
                                                        newChan, readChan,
                                                        tryWriteChan)
import           Control.Monad                         (forever, void)
import           Data.Semigroup                        ((<>))
import           Options.Applicative                   (Parser, ParserInfo (..),
                                                        argument, execParser,
                                                        fullDesc, header, help,
                                                        helper, info, long,
                                                        metavar, progDesc,
                                                        short, str, switch,
                                                        (<**>))
import           System.Exit                           (exitSuccess)
import           System.INotify                        (Event,
                                                        EventVariety (..),
                                                        INotify,
                                                        WatchDescriptor,
                                                        addWatch, initINotify,
                                                        removeWatch)

data Job = Job
    { chans   :: (InChan Event, OutChan Event)
    , inotify :: INotify
    -- Might need some kind of collection to keep track of WatchDescriptors.
    }

instance Show Job where
  show _ = "Job (ic, oc) stream"

data Args = Args
  { target    :: String
  , command   :: String
  , recursive :: Bool
  } deriving (Show)

progArgs :: Parser Args
progArgs = Args
  <$> argument str
      ( metavar "TARGET"
     <> help "Target to watch" )
  <*> argument str
      ( metavar "COMMAND"
     <> help "Command to run on file changes" )
  <*> switch
      ( long "recursive"
     <> short 'r'
     <> help "Watch subdirectories recursively" )

progInfo :: ParserInfo Args
progInfo = info (progArgs <**> helper)
              ( fullDesc
             <> progDesc "Watch TARGET and run COMMAND on changes"
             <> header "stickybeak" )

subscribe :: Job -> Args -> IO Job
subscribe Job{..} Args{..} = do
    wd <- addWatch inotify [Modify] target (\evt -> void (tryWriteChan ic evt))
    return $ Job chans inotify
  where
    (ic, _) = chans

newJob :: IO Job
newJob = do
  chans <- newChan 1
  inotify <- initINotify
  return $ Job chans inotify

newWorker :: Job -> IO ThreadId
newWorker job = forkIO . forever $ readChan oc >>= print
  where
    (_, oc) = chans job


stickybeak :: IO ()
stickybeak = do
  args <- execParser progInfo
  job <- newJob
  worker <- newWorker job
  subscribe job args
  _ <- getLine
  exitSuccess
