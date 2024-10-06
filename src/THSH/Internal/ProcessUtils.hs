module THSH.Internal.ProcessUtils
  ( binaryCat, binaryCat'
  , pollProcessExitCode
  ) where

import           Control.Concurrent (threadDelay, yield)
import           Control.Monad      (unless)
import           Foreign            (Ptr, allocaBytes)
import           System.Exit        (ExitCode)
import           System.IO
import           System.Process     (ProcessHandle, getProcessExitCode)


binaryCat :: Handle -> Handle -> IO ()
binaryCat hr hw = allocaBytes cBUFFER_SIZE (binary_cat_with_ptr "" hr hw)

binaryCat' :: String -> Handle -> Handle -> IO ()
binaryCat' l hr hw = allocaBytes cBUFFER_SIZE (binary_cat_with_ptr l hr hw)

pollProcessExitCode :: ProcessHandle -> IO ExitCode
pollProcessExitCode ph = getProcessExitCode ph >>= \case
  Nothing -> yield >> threadDelay cPROCESS_POLL_INTERVAL >> pollProcessExitCode ph
  Just ec -> pure ec

{- INTERNAL FUNCTIONS -}

cPROCESS_POLL_INTERVAL :: Int
cPROCESS_POLL_INTERVAL = 10_000

-- https://stackoverflow.com/questions/68639266/size-of-buffered-input-in-c
-- By many accounts, it seems "gnu cat" uses 128 KB as buffer size
cBUFFER_SIZE :: Int
cBUFFER_SIZE = 128 * 1024

binary_cat_with_ptr :: String -> Handle -> Handle -> Ptr a -> IO ()
binary_cat_with_ptr l hr hw ptr = go where
  go = do
    unless (length l == 0) (put_err_ln $ "!! cat " <> l <> " go")
    n <- hGetBufSome hr ptr cBUFFER_SIZE
    unless (length l == 0) (put_err_ln $ "!! cat " <> l <> " n " <> show n)
    unless (n == 0) $ hPutBuf hw ptr n >> go

put_err_ln :: String -> IO ()
put_err_ln = hPutStrLn stderr
