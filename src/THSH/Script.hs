{-# LANGUAGE QuasiQuotes #-}
module THSH.Script
  ( Script (..)
  , genFuncletPipeCode
  , sh
  ) where

import           Control.Concurrent (forkIO)
import           Control.Monad      (void)
import           Data.Function      ((&))
import           System.Exit        (ExitCode)
import           System.FilePath    ((</>))
import           System.IO          (IOMode (..), hPutStr, withFile)
import           System.IO.Temp     (withSystemTempDirectory)
import           System.Process     (CreateProcess (..), ProcessHandle, StdStream (..), callCommand, createProcess,
                                     getProcessExitCode, shell)
-- PyF
import           PyF                (fmt, str)
--
import           THSH.Funclet       (AnyFunclet, Funclet (..), runFuncletWithFifos)


data Script = MkScript { source   :: String
                       , funclets :: [AnyFunclet]
                       }
            -- TODO deriving Show

instance Funclet Script where
  runFunclet (MkScript { source, funclets }) (hIn, hOut, hErr) = withSystemTempDirectory "thsh-script.d" $ \ dir ->
    let initCodePath = dir </> "init.sh"
        srcPath = dir </> "source.sh"
    in do
      -- write init code
      withFile initCodePath WriteMode $ \fh -> hPutStr fh (genInitCode (length funclets))
      -- call init code
      callCommand ("sh " <> initCodePath)
      -- write source file
      withFile srcPath WriteMode $ \fh -> do
        hPutStr fh genStubCode
        hPutStr fh source
      -- create the main process
      (_, _, _, mp) <- createProcess $
                       (shell ("sh " <> srcPath)) { std_in = UseHandle hIn
                                                  , std_out = UseHandle hOut
                                                  , std_err = UseHandle hErr
                                                  -- , delegate_ctlc = True
                                                  }
      -- create sub processes for funclets
      mapM_
            (\(i, f) -> forkIO (runFuncletWithFifos f (nthFifos dir i) & void))
            (zip [(0 :: Integer)..] funclets)
      -- wait for the main sub process to finish
      ec <- pollProcessExitCode mp
      pure ec
        where
          -- nthFifos should agree with initCode on the fifo file names
          nthFifos dir i = let d = dir </> (show i) in (d </> "0.fifo", d </> "1.fifo", d </> "2.fifo")

genFuncletPipeCode :: Int -> String
genFuncletPipeCode i = "__pipeFunclet " <> (show i)

sh :: Script -> Script
sh = id

{- INTERNAL FUNCTIONS -}

genInitCode :: Int -> String
genInitCode nFunclets = [str|\
# set -x
__initFunclets() {
  n="$1"
  [ "$n" == "-1" ] && return
  d="$(dirname "$0")"
  seq 0 "$n" | while read i; do
    mkdir -p "$d/$i"
    mkfifo "$d/$i/0.fifo" "$d/$i/1.fifo" "$d/$i/2.fifo"
  done
}
|] <> [fmt|__initFunclets {show (nFunclets - 1)}
|]

genStubCode :: String
genStubCode = [str|\
#+BEGIN_SRC THSH script stub code
# set -x
__pipeFunclet() {
  # connect to the fifos of nth functlet
  i="$1"
  d="$(dirname "$0")/$i"
  command > "$d"/0.fifo
  cat >&1 "$d"/1.fifo &
  cat >&2 "$d"/2.fifo &
  cat >   "$d"/0.fifo &
}
#+END_SRC
|]

pollProcessExitCode :: ProcessHandle -> IO ExitCode
pollProcessExitCode ph = getProcessExitCode ph >>= \case
  Nothing -> pollProcessExitCode ph
  Just ec -> pure ec
