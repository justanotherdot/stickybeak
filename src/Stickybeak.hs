module Stickybeak
  ( stickybeak
  , subdirectories
  ) where

import           Control.Concurrent.STM (TMVar)
import qualified Control.Concurrent.STM as STM
import           Control.Monad          (forever)
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
                                         removeWatch, killINotify)
import           System.Process         (ProcessHandle, spawnCommand)
import qualified System.IO as IO

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

subscribe :: INotify
          -> TMVar JobMap
          -> String -- target
          -> String -- command
          -> IO WatchDescriptor
subscribe inotify jobMap tgt cmd = addWatch inotify [CloseWrite] tgt eventHandler
  where
    eventHandler _ = do
      ts <- getCurrentTime
      (jm, needsUpdate) <- STM.atomically $ do
        jm' <- STM.takeTMVar jobMap
        case Map.lookup cmd jm' of
          Nothing          -> return (jm', True)
          Just (Job _ ts') -> return (jm', diffUTCTime ts ts' > defaultDebounce)
      if needsUpdate
         then do
           ph <- spawnCommand cmd
           STM.atomically $ STM.putTMVar jobMap $ Map.insert cmd (Job ph ts) jm
         else STM.atomically $ STM.putTMVar jobMap jm

-- | Get all the subdirectories of a given directory.
--
-- n.b. Will ignore hidden directories and files
-- but will allow passing "." as an initial arg.
subdirectories :: FilePath -> IO [FilePath]
subdirectories dir = do
    isDir <- doesDirectoryExist dir
    if isDir
       then do
         fs   <- (prefixDir . filter isHidden) <$> listDirectory dir
         dirs <- concat <$> traverse subdirectories fs
         return $ dir : dirs
       else
         return []
  where prefixDir = map (\fn -> dir <> "/" <> fn)

        head' xs
          | null xs   = Nothing
          | otherwise = let (x:_) = xs
                         in Just x

        isHidden fn = case head' fn of
                       Just c  -> c /= '.'
                       Nothing -> False

stickybeak :: IO ()
stickybeak = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  IO.hSetBuffering IO.stdin  IO.NoBuffering
  args <- execParser progInfo
  jobMap <- STM.atomically $ STM.newTMVar Map.empty
  inotify <- initINotify
  if not (recursive args)
     then do
       wd <- subscribe inotify jobMap (target args) (command args)
       exitLoop (removeWatch wd >> killINotify inotify >> exitSuccess)
     else do
       fs  <- subdirectories (target args)
       wds <- traverse (\fn -> subscribe inotify jobMap fn (command args)) fs
       exitLoop (traverse removeWatch wds >> killINotify inotify >> exitSuccess)
  where exitLoop cleanup =
          let check = \s -> if s == 'q' then cleanup else return ()
           in forever (getChar >>= check)
