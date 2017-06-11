{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}

module Stickybeak
  ( stickybeak
  , runIO
  , runPure
  -- XXX Need to move these into an Internals module!
  , Stickybeak
  , subscribe
  , unsubscribe
  , checkArgs
  , exit
  ) where

{- import           System.INotify                        (INotify, WatchDescriptor) -}
import           Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan,
                                                        newChan)
import           Control.Monad.Free
import           Data.Semigroup                        ((<>))
import           Options.Applicative                   (Parser, ParserInfo (..),
                                                        argument, execParser,
                                                        fullDesc, header, help,
                                                        helper, info, long,
                                                        metavar, progDesc,
                                                        short, str, switch,
                                                        (<**>))
import           System.Exit                           (exitSuccess)

data Worker = forall ic oc s. Worker
    { chans  :: (ic, oc)
    , stream :: Maybe s
    }

instance Show Worker where
  show _ = "Worker (ic, oc) stream"

data Job = Job
  { worker   :: Worker
  , progArgs :: Args
  } deriving (Show)

data Args = Args
  { target    :: String
  , command   :: String
  , recursive :: Bool
  } deriving (Show)

argParser :: Parser Args
argParser = Args
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
progInfo = info (argParser <**> helper)
      ( fullDesc
     <> progDesc "Watch TARGET and run COMMAND on changes"
     <> header "stickybeak" )

data StickybeakF next where
  MakeWorker  :: (Worker -> next) -> StickybeakF next
  CheckArgs   :: (Args -> next) -> StickybeakF next
  Exit        :: StickybeakF next
  Subscribe   :: Worker -> Args -> (Job -> next) -> StickybeakF next
  Unsubscribe :: Job -> next -> StickybeakF next
  deriving (Functor)

type Stickybeak = Free StickybeakF

makeWorker :: Stickybeak Worker
makeWorker = liftF $ MakeWorker id

checkArgs :: Stickybeak Args
checkArgs = liftF $ CheckArgs id

exit :: Stickybeak r
exit = liftF Exit

unsubscribe :: Job -> Stickybeak ()
unsubscribe s = liftF $ Unsubscribe s ()

subscribe :: Worker -> Args -> Stickybeak Job
subscribe w as = liftF $ Subscribe w as id

runIO :: Stickybeak r -> IO r
runIO (Free (Subscribe w as f))  = return (Job w as) >>= runIO . f
runIO (Free (Unsubscribe job n)) = print job >> runIO n
runIO (Free (CheckArgs f))       = execParser progInfo >>= runIO . f
-- | TODO Change this hardcoded `1` to a value that's variable for async jobs.
runIO (Free (MakeWorker n))      = do
  (ic, oc) <- newChan 1
  return (Worker (ic, oc) (Just "wd")) >>= runIO . n
runIO (Free Exit)                = exitSuccess
runIO (Pure r)                   = return r

runPure :: Stickybeak r -> Either String r
runPure (Free (Subscribe w as n))  = return (Job w as) >>= runPure . n
runPure (Free (Unsubscribe job n)) = return job >> runPure n
-- | TODO N.B. We can use execParserPure here for testing purposes (e.g. writing arbitrary instances).
runPure (Free (CheckArgs n))       = return (Args "blah" "blah" True) >>= runPure . n
runPure (Free (MakeWorker n))      = return (Worker ("ic", "oc") (Just "wd")) >>= runPure . n
runPure (Free Exit)                = Left "Exit"
runPure (Pure r)                   = return r

stickybeak :: Stickybeak ()
stickybeak = do
  w  <- makeWorker
  as <- checkArgs
  j  <- subscribe w as
  unsubscribe j
  exit
