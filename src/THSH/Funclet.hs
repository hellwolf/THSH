module THSH.Funclet
  ( Funclet (..)
  , AnyFunclet (..)
  , runFuncletWithStdHandles
  , runFuncletWithFifos
  ) where


import           GHC.IO.Handle     (Handle)
import           GHC.IO.StdHandles (stderr, stdin, stdout)
import           System.Exit       (ExitCode)
import           System.IO         (IOMode (..), SeekMode (..), hPutStrLn, hSeek, openFile, withFile)


class Funclet f where
  runFunclet :: f -> (Handle, Handle, Handle) -> IO ExitCode

data AnyFunclet = forall f. Funclet f => MkAnyFunclet f

instance Funclet AnyFunclet where
  runFunclet (MkAnyFunclet f) = runFunclet f

runFuncletWithStdHandles :: Funclet f => f -> IO ExitCode
runFuncletWithStdHandles f = runFunclet f (stdin, stdout, stderr)

runFuncletWithFifos :: Funclet f => f -> (FilePath, FilePath, FilePath) -> IO ExitCode
runFuncletWithFifos f (fifoIn, fifoOut, fifoErr) = do
  -- hPutStrLn stderr ("fifoIn " <> fifoIn)
  -- hPutStrLn stderr ("fifoOut " <> fifoOut)
  -- hIn <- openFile fifoIn ReadMode
  -- hOut <- openFile fifoOut ReadWriteMode
  -- hErr <- openFile fifoErr ReadWriteMode
  -- Note: for some reason, neither WriteMode nor AppendMode works for the fifo files
  withFile fifoIn ReadMode $ \ hIn -> withFile fifoOut ReadWriteMode $ \ hOut -> withFile fifoErr ReadWriteMode $ \ hErr -> do
    -- hSeek hIn AbsoluteSeek 0
    runFunclet f (hIn, hOut, hErr)
