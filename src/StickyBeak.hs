{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}

module Stickybeak
  ( echo
  , runIO
  ) where

{- import           Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan) -}
import           Control.Monad.Free
import           System.Exit        (exitSuccess)
{- import           System.INotify                        (INotify, -}
                                                        {- WatchDescriptor) -}

{- data Job where -}
  {- Job :: (InChan, OutChan) -> INotify -> Maybe WatchDescriptor -> Job  -}

{- data Job = Job -}
  {- { channels :: (InChan, OutChan) -}
  {- , inotify  :: INotify -}
  {- , wd       :: Maybe WatchDescriptor -}
  {- } -}

{- data Job where -}
  {- Job :: Job -}

{- data Stickybeak n where -}
  {- Subscribe :: Job -> FilePath -> (a -> IO ()) -> IO () -}

data StickybeakF x where
  {- CheckArgs   :: StickybeakF x -}
  Subscribe   :: (String -> x) -> StickybeakF x
  Unsubscribe :: String -> x -> StickybeakF x
  ExitSuccess :: StickybeakF x
  {- ExitFailure :: StickybeakF x -}
  deriving (Functor)

type Stickybeak = Free StickybeakF

unsubscribe :: String -> Stickybeak ()
unsubscribe s = liftF $ Unsubscribe s ()

subscribe :: Stickybeak String
subscribe = liftF $ Subscribe id

exitSuccess' :: Stickybeak r
exitSuccess' = liftF ExitSuccess

runIO :: Stickybeak r -> IO r
runIO (Pure r)                 = return r
runIO (Free (Unsubscribe s n)) = putStrLn s >> runIO n
runIO (Free (Subscribe f))     = getLine >>= runIO . f
runIO (Free ExitSuccess)       = exitSuccess

echo :: Stickybeak ()
echo = do
  str <- subscribe
  unsubscribe str
  _ <- exitSuccess'
  unsubscribe str
