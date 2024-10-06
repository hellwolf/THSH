module THSH.Funclet
  ( Funclet (..)
  , AnyFunclet (..)
  , runFuncletWithStdHandles
  ) where

import           Control.Concurrent         (forkIO, newEmptyMVar, putMVar, takeMVar)
import           System.Exit                (ExitCode)
import           System.IO                  (Handle, stderr, stdin, stdout)
--
import           THSH.Internal.ProcessUtils (binaryCat)


-- | A funclet is an IO process that communicates through handles and returns an exit code.
class Funclet f where
  runFunclet :: f -> (ExitCode -> IO ()) -> IO (Handle, Handle, Handle)

-- | Run a funclet with standard handles
runFuncletWithStdHandles :: Funclet f => f -> IO ExitCode
runFuncletWithStdHandles f = do
  ecVar <- newEmptyMVar
  (hInW, hOutR, hErrR) <- runFunclet f (putMVar ecVar)
  mapM_ forkIO [ binaryCat stdin hInW
               , binaryCat hOutR stdout
               , binaryCat hErrR stderr
               ]
  ec <- takeMVar ecVar
  pure ec

-- | Existential wrapper of any funclet.
data AnyFunclet = forall f. Funclet f => MkAnyFunclet f

instance Funclet AnyFunclet where
  runFunclet (MkAnyFunclet f) = runFunclet f
