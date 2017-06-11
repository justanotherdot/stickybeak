{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Stickybeak
  ( stickybeak
  , runIO
  , runPure
  , Stickybeak
  , subscribe
  , unsubscribe
  , checkArgs
  , exit
  ) where

{- import           Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan) -}
{- import           System.INotify                        (INotify, WatchDescriptor) -}
import           Control.Monad.Free
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Exit         (exitSuccess)

data Job = Job
  { chans   :: (String, String)
  , inotify :: String
  , wd      :: String
  } deriving (Show)

data Args = Args
  { target    :: String
  , command   :: String
  , recursive :: Bool
  } deriving (Show)

argParser :: Parser Args
argParser = Args
  <$> strOption
      ( long "target"
     <> short 't'
     <> metavar "TARGET"
     <> help "Target to watch" )
  <*> strOption
      ( long "command"
     <> short 'c'
     <> metavar "COMMAND"
     <> help "Command to run on file changes" )
  <*> switch
      ( long "recursive"
     <> short 'r'
     <> help "Watch subdirectories recursively" )

data StickybeakF next where
  CheckArgs   :: (Args -> next) -> StickybeakF next
  Exit        :: StickybeakF next
  Subscribe   :: Args -> (Job -> next) -> StickybeakF next
  Unsubscribe :: Job -> next -> StickybeakF next
  deriving (Functor)

type Stickybeak = Free StickybeakF

checkArgs :: Stickybeak Args
checkArgs = liftF $ CheckArgs id

exit :: Stickybeak r
exit = liftF Exit

unsubscribe :: Job -> Stickybeak ()
unsubscribe s = liftF $ Unsubscribe s ()

subscribe :: Args -> Stickybeak Job
subscribe args = liftF $ Subscribe args id

runIO :: Stickybeak r -> IO r
runIO (Free (Subscribe args f))  = return (Job ("ic", "oc") "in" "wd") >>= runIO . f
runIO (Free (Unsubscribe job n)) = print job >> runIO n
runIO (Free (CheckArgs f))       = return (Args "blah" "blah" True) >>= runIO . f
runIO (Free Exit)                = exitSuccess
runIO (Pure r)                   = return r

runPure :: Stickybeak r -> Either String r
runPure (Free (Subscribe args n))  = return (Job ("ic", "oc") "in" "wd") >>= runPure . n
runPure (Free (Unsubscribe job n)) = return (Job ("","") "" "") >> runPure n
runPure (Free (CheckArgs n))       = return (Args "blah" "blah" True) >>= runPure . n
runPure (Free Exit)                = Left "Exit"
runPure (Pure r)                   = return r

stickybeak :: Stickybeak ()
stickybeak = do
  args <- checkArgs
  job <- subscribe args
  unsubscribe job
  exit
