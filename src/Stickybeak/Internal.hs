{-# LANGUAGE CPP #-}

module Stickybeak.Internal
  ( subscribe
  ) where

data Job = Job ProcessHandle UTCTime

instance Show Job where
  show (Job _ ts) = show ts

type JobMap = Map String Job

defaultDebounce :: RealFrac a => a
defaultDebounce = 0.250

subscribe :: INotify -> TMVar JobMap -> Args -> IO WatchDescriptor
subscribe inotify jobMap args = addWatch inotify [CloseWrite] (target args) eventHandler
  where eventHandler = updateJobMap jobMap

-- Ignores the event for now, but later we'll want to take
-- the Event and filter for file information based on its content.
updateJobMap :: TMVar JobMap -> Event -> IO ()
updateJobMap jobMap _ = do
      ts <- getCurrentTime
      (jm, needsUpdate) <- atomically $ do
        jm' <- takeTMVar jobMap
        case Map.lookup (command args) jm' of
          Nothing          -> return (jm', True)
          Just (Job _ ts') -> return (jm', diffUTCTime ts ts' > defaultDebounce)
      if needsUpdate
         then do
           ph <- spawnCommand (command args)
           atomically $ putTMVar jobMap $ Map.insert (command args) (Job ph ts) jm
          else atomically $ putTMVar jobMap jm
