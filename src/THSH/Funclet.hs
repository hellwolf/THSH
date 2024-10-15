{-|
Module      : THSH.Funclet
Description : Funclet definition.
Copyright   : (c) Miao ZhiCheng, 2024
License     : MIT
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : POSIX
-}

module THSH.Funclet
  ( Funclet (..)
  , AnyFunclet (..)
  , runFuncletWithStdHandles
  ) where

-- base module
import           Control.Concurrent         (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception          (bracket)
import           System.Exit                (ExitCode)
import           System.IO                  (BufferMode (NoBuffering), Handle, hClose, hSetBuffering, stderr, stdin,
                                             stdout)
--
import           System.Process             (createPipe)
--
import           THSH.Internal.ProcessUtils (binaryCat)


-- | A funclet is an IO process that communicates through handles and calls back with an exit code.
class Funclet f where
  {-# MINIMAL runFunclet | runFuncletWithHandles #-}

  -- | Run the funclet which creates a set of handles itself.
  runFunclet :: f -> (ExitCode -> IO ()) -> IO (Handle, Handle, Handle)
  -- ^ It has a default implementation that calls `runFuncletWithHandles` with created pipes.
  runFunclet f cb = do
    handles <- newEmptyMVar
    _ <- forkIO $ bracket
      (do
          (hInR, hInW)   <- createPipe
          (hOutR, hOutW) <- createPipe
          (hErrR, hErrW) <- createPipe
          mapM_ (`hSetBuffering` NoBuffering) [hInR, hInW, hOutR, hOutW, hErrR, hErrW]
          putMVar handles (hInW, hOutR, hErrR)
          pure (hInR, hOutW, hErrW)
      )
      (\(hInR, hOutW, hErrW) -> mapM_ hClose [hInR, hOutW, hErrW])
      (\(hInR, hOutW, hErrW) -> runFuncletWithHandles f cb (hInR, hOutW, hErrW))
    takeMVar handles

  -- | Run the funclet with the set of handles provided.
  runFuncletWithHandles :: f -> (ExitCode -> IO ()) -> (Handle, Handle, Handle) -> IO ()
  -- ^ It has a default implementation that simply piping data between `runFunclet` and the provided handles.
  runFuncletWithHandles f cb (hInR, hOutW, hErrW) = do
    (hInW, hOutR, hErrR) <- runFunclet f cb
    mapM_ forkIO [ binaryCat hInR hInW
                 , binaryCat hOutR hOutW
                 , binaryCat hErrR hErrW
               ]

-- | Run a 'Funclet' with standard handles synchronously.
runFuncletWithStdHandles :: Funclet f => f -> IO ExitCode
runFuncletWithStdHandles f = do
  mExitCode <- newEmptyMVar
  runFuncletWithHandles f (putMVar mExitCode) (stdin, stdout, stderr)
  takeMVar mExitCode

-- | Existential wrapper of any 'Funclet'.
data AnyFunclet = forall f. Funclet f => MkAnyFunclet f

-- | 'AnyFunclet' is of course also a 'Funclet'.
instance Funclet AnyFunclet where
  runFunclet (MkAnyFunclet f) = runFunclet f
