module THSH.SubProc where

import           GHC.IO.Handle  (Handle)
import           System.IO      (hFlush, hPutStr)
import           System.IO.Temp (withSystemTempFile)
import           System.Process (createProcess, shell, waitForProcess)

data SubProcess = SubProcess
  { stdinFdPath  :: String
  , stdoutFdPath :: String
  }

runSH :: String -> IO SubProcess
runSH script = withSystemTempFile "thsh.sh" $ \ fpath fh -> do
  hPutStr fh script
  hFlush fh
  (_, _, _, ph) <- createProcess (shell ("sh " <> fpath))
  _ <- waitForProcess ph
  pure (SubProcess "in" "out")
