module Stickybeak
  ( stickybeak
  ) where

{- import           Control.Concurrent.Async -}
import           Control.Concurrent.STM
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Semigroup         ((<>))
import           Data.Time              (UTCTime (..), diffUTCTime,
                                         getCurrentTime)
import           Options.Applicative    (Parser, ParserInfo (..), argument,
                                         execParser, fullDesc, header, help,
                                         helper, info, long, metavar, progDesc,
                                         short, str, switch, (<**>))
import           System.Directory       (doesDirectoryExist, listDirectory)
import           System.Exit            (exitSuccess)
import           System.INotify         (EventVariety (..), INotify,
                                         WatchDescriptor, addWatch, initINotify,
                                         removeWatch)
import           System.Process         (ProcessHandle, spawnCommand)

data Job = Job ProcessHandle UTCTime

instance Show Job where
  show (Job _ ts) = show ts

type JobMap = Map String Job

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

defaultDebounce :: RealFrac a => a
defaultDebounce = 0.250

subscribe :: INotify -> TMVar JobMap -> Args -> IO WatchDescriptor
subscribe inotify jobMap args = addWatch inotify [CloseWrite] (target args) eventHandler
  where
    eventHandler _ = do
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

-- Notes:
--   * Use a map as an accumulator for efficiency.
--   * Compress the `if-then-else` into just combinators.
--   * Will not ignore hidden directories.
subdirectories :: FilePath -> IO [FilePath]
subdirectories dir = do
  isDir <- doesDirectoryExist dir
  if isDir
     then do
       fs <- map (dirSlash <>) <$> listDirectory dir
       dirs <- concat <$> traverse subdirectories fs
       return $ dir : dirs
     else
       return []
  where dirSlash = dir <> "/"

stickybeak :: IO ()
stickybeak = do
  args <- execParser progInfo
  jobMap <- atomically $ newTMVar Map.empty
  inotify <- initINotify
  wd <- subscribe inotify jobMap args
  _ <- getLine
  removeWatch wd
  exitSuccess
