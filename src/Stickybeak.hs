{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}

module Stickybeak
  ( echo
  , echo'
  , runIO
  , runPure
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

data StickybeakF next where
  CheckArgs   :: StickybeakF next
  Exit        :: StickybeakF next
  Subscribe   :: (Job -> next) -> StickybeakF next
  Unsubscribe :: Job -> next -> StickybeakF next
  deriving (Functor)

type Stickybeak = Free StickybeakF

unsubscribe :: Job -> Stickybeak ()
unsubscribe s = liftF $ Unsubscribe s ()

subscribe :: Stickybeak Job
subscribe = liftF $ Subscribe id

exit :: Stickybeak r
exit = liftF Exit

runIO :: Stickybeak r -> IO r
runIO (Free (Subscribe f))     = return (Job ("ic", "oc") "in" "wd") >>= runIO . f
runIO (Free (Unsubscribe j n)) = print j >> runIO n
runIO (Free CheckArgs)         = exitSuccess
runIO (Free Exit)              = exitSuccess
runIO (Pure r)                 = return r

runPure :: Stickybeak r -> Maybe r
runPure (Free (Subscribe n))     = return (Job ("ic", "oc") "in" "wd") >>= runPure . n
runPure (Free (Unsubscribe _ n)) = runPure n
runPure (Free CheckArgs)         = Nothing
runPure (Free Exit)              = Nothing
runPure (Pure r)                 = return r

echo :: Stickybeak ()
echo = do
  job <- subscribe
  unsubscribe job
  _ <- exit
  unsubscribe job -- Make sure it deos not print again.

echo' :: Stickybeak Job
echo' = do
  job <- subscribe
  return job
