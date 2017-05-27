{-# LANGUAGE GADTs #-}

module StickyBeak (runIO, StickyBeak(..)) where

import           Control.Monad.Free

{- data JobExecutor where -}
  {- JobExecutor :: (InChan, OutChan) -> -}

{- data StickyBeak n where -}
  {- Subscribe :: JobExecutor -> FilePath -> (a -> IO ()) -> IO () -}

data StickyBeakF f where
  CheckArgs   :: StickyBeakF f
  Subscribe   :: StickyBeakF f
  Unsubscribe :: StickyBeakF f
  ExitSuccess :: StickyBeakF f
  ExitFailure :: StickyBeakF f

type StickyBeak = Free StickyBeakF

runIO :: StickyBeak () -> IO ()
runIO _ = putStrLn "Not Implemented"
