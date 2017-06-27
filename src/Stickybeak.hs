{-# LANGUAGE RecordWildCards #-}

module Stickybeak
  ( stickybeak
  ) where

import           Control.Concurrent           (ThreadId, forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar
import           Control.Monad                (forever, void)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Semigroup               ((<>))
import           Data.Time                    (UTCTime (..), getCurrentTime)
import           Options.Applicative          (Parser, ParserInfo (..),
                                               argument, execParser, fullDesc,
                                               header, help, helper, info, long,
                                               metavar, progDesc, short, str,
                                               switch, (<**>))
import           System.Exit                  (exitSuccess)
import           System.INotify               (Event, EventVariety (..),
                                               INotify, WatchDescriptor,
                                               addWatch, initINotify,
                                               removeWatch)
import           System.Process               (ProcessHandle, readCreateProcess,
                                               shell)

data Job = Job ProcessHandle UTCTime
type JobMap = Map String Job

data Args = Args
  { target    :: String
  , command   :: String
  , recursive :: Bool
  } deriving (Show)

progArgs :: Parser Args
progArgs =
  Args <$> argument str (metavar "TARGET" <> help "Target to watch") <*>
  argument str (metavar "COMMAND" <> help "Command to run on file changes") <*>
  switch
    (long "recursive" <> short 'r' <> help "Watch subdirectories recursively")

progInfo :: ParserInfo Args
progInfo =
  info
    (progArgs <**> helper)
    (fullDesc <> progDesc "Watch TARGET and run COMMAND on changes" <>
     header "stickybeak")

{- subscribe :: Job -> Args -> IO Job -}
{- subscribe Job {..} Args {..} = do -}
  {- wd <- addWatch inotify [CloseWrite] target eventHandler -}
  {- return $ Job inChan outChan inotify -}
  {- where -}
    {- eventHandler evt = void (tryWriteChan inChan evt) -}

{- newJob :: IO Job -}
{- newJob = do -}
  {- (ic, oc) <- newChan 1 -}
  {- inotify <- initINotify -}
  {- return $ Job ic oc inotify -}

{- -- TODO change this from forkIO to async, that way we can `cancel` -}
{- -- instead of using `getLine` -}
{- newWorker :: Job -> Args -> IO ThreadId -}
{- newWorker Job {..} Args {..} = forkIO $ forever cmd -}
  {- {- where cmd = readChan outChan >> readCreateProcess (shell command) "" >>= putStr -} -}
  {- where -}
    {- cmd = readChan outChan >>= print -}



stickybeak :: IO ()
stickybeak = do
  args <- execParser progInfo
  exitSuccess
