module Error (exitFailureMsg) where

import           System.Exit (exitFailure)
import           System.IO   (hPutStrLn, stderr)

exitFailureMsg :: String -> IO a
exitFailureMsg msg = hPutStrLn stderr msg >> exitFailure
