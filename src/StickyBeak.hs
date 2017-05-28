{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}

module StickyBeak where

import           Control.Monad.Free

{- data JobExecutor where -}
  {- JobExecutor :: (InChan, OutChan) -> -}

data Job where
  Job :: Job

{- data StickyBeak n where -}
  {- Subscribe :: JobExecutor -> FilePath -> (a -> IO ()) -> IO () -}

data StickyBeakF f where
  CheckArgs   :: StickyBeakF f
  Subscribe   :: (Job -> FilePath -> f) -> StickyBeakF f
  Unsubscribe :: StickyBeakF f
  ExitSuccess :: StickyBeakF f
  ExitFailure :: StickyBeakF f
  deriving (Functor)

type StickyBeak = Free StickyBeakF

runIO :: StickyBeak () -> IO ()
runIO _ = putStrLn "Not Implemented"
