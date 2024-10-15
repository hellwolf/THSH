{-|
Module      : THSH.Script
Description : Script funclet create a system process for the shell script.
Copyright   : (c) Miao ZhiCheng, 2024
License     : MIT
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE QuasiQuotes #-}
module THSH.Script
  ( Script (..)
  , genFuncletPipeCode
  , sh
  ) where

-- base module
import           Control.Concurrent         (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception          (bracket, catch)
import           Control.Monad              (unless, void)
import           Data.Function              ((&))
import           Data.Maybe                 (fromJust)
import           System.Exit                (ExitCode (..))
import           System.IO                  (BufferMode (NoBuffering), Handle,
                                             IOMode (ReadMode, ReadWriteMode, WriteMode), hClose, hGetLine, hPutStr,
                                             hPutStrLn, hSetBuffering, openBinaryFile, stderr, withFile)
-- filepath module
import           System.FilePath            ((</>))
-- process module
import           System.Process             (CreateProcess (..), StdStream (CreatePipe, UseHandle), callCommand,
                                             createProcess, shell)
-- temporary module
import           System.IO.Temp             (withSystemTempDirectory)
-- PyF module
import           PyF                        (fmt, str)
--
import           THSH.Funclet               (AnyFunclet, Funclet (..))
import           THSH.Internal.ProcessUtils (binaryCat, pollProcessExitCode)


-- | A script contains shell source code and a list of other funclets it depends on.
data Script = MkScript { source   :: String
                       , funclets :: [AnyFunclet]
                       }

-- | The marker for the 'thsh' quasi-quote to recognize a 'Script'.
sh :: Script -> Script
sh = id

-- | The 'Script' instance of 'Funclet'.
instance Funclet Script where
  runFunclet script cb = run_script_funclet script cb Nothing >>= pure . fromJust

  runFuncletWithHandles script cb providedHandles = void (run_script_funclet script cb (Just providedHandles))

-- | The piping code snippet that should substitute the funclet occurrences during quasi quoting.
genFuncletPipeCode :: Int -> String
genFuncletPipeCode i = "__pipeFunclet " <> (show i)

{- INTERNAL FUNCTIONS -}

run_script_funclet :: Script -> (ExitCode -> IO ()) -> Maybe (Handle, Handle, Handle)
                   -> IO (Maybe (Handle, Handle, Handle))
run_script_funclet (MkScript { source, funclets }) cb providedHandles = do
  mProcHandles <- newEmptyMVar

  _ <- forkIO $ withSystemTempDirectory "thsh-script.d" $ \ dir -> do
        let initCodePath = dir </> "init.sh"
            srcPath      = dir </> "source.sh"
            ctlFifo      = dir </> "cr.fifo"

        -- write init code
        withFile initCodePath WriteMode $ \fh -> hPutStr fh (gen_init_code (length funclets))

        -- call init code
        callCommand ("sh " <> initCodePath)

        -- write source file
        withFile srcPath WriteMode $ \fh -> do
          hPutStr fh gen_stub_code
          hPutStr fh source

        -- create the shell script process
        mMainProc <- newEmptyMVar
        -- TODO: I can't make this work with withCreateProcess
        -- withCreateProcess
        --   (shell ("sh " <> srcPath) & \ procSpec -> case providedHandles of
        --       Just (hInR, hOutW, hErrW) -> procSpec { std_in  = UseHandle hInR
        --                                             , std_out = UseHandle hOutW
        --                                             , std_err = UseHandle hErrW
        --                                             }
        --       Nothing                   -> procSpec { std_in  = CreatePipe
        --                                             , std_out = CreatePipe
        --                                             , std_err = CreatePipe
        --                                             }
        --   )
        --   (\ cases
        --     (Just hInW) (Just hOutR) (Just hErrR) mainProc -> putMVar mProcHandles (Just (hInW, hOutR, hErrR))
        --                                                       >> putMVar mMainProc mainProc
        --     _ _ _                                 mainProc -> putMVar mProcHandles Nothing
        --                                                       >> putMVar mMainProc mainProc
        --   )
        createProcess
          (shell ("sh " <> srcPath) & \ procSpec -> case providedHandles of
              Just (hInR, hOutW, hErrW) -> procSpec { std_in  = UseHandle hInR
                                                    , std_out = UseHandle hOutW
                                                    , std_err = UseHandle hErrW
                                                    }
              Nothing                   -> procSpec { std_in  = CreatePipe
                                                    , std_out = CreatePipe
                                                    , std_err = CreatePipe
                                                    }
          )
          >>= (\ cases
                (Just hInW, Just hOutR, Just hErrR, mainProc) -> putMVar mProcHandles (Just (hInW, hOutR, hErrR))
                                                                 >> putMVar mMainProc mainProc
                (_, _, _,                           mainProc) -> putMVar mProcHandles Nothing
                                                                 >> putMVar mMainProc mainProc
          )

        -- create control thread
        unless (length funclets == 0) $ void . forkIO $ withFile ctlFifo ReadMode $ \ ch -> let
          go = do
            catch
              (hGetLine ch)
              (\ (e :: IOError) -> hPutStrLn stderr ("THSH.Script control thread error: " <> show e) >> pure [])
              >>= \cmd -> do
              case cmd of
                []        -> pure () -- likely end of file
                's':' ':i -> start_funclet_proc (funclets !! (read i :: Int)) (dir </> i) >> pure ()
                _         -> hPutStrLn stderr $ "[thsh-script] unknown control command: " <> cmd
              if cmd /= "" then go else pure ()
          in go

        catch
          (takeMVar mMainProc >>= pollProcessExitCode >>= cb)
          (\ (e :: IOError) -> hPutStrLn stderr ("THSH.Script funclet thread error: " <> show e)
                               >> cb (ExitFailure 2))

  takeMVar mProcHandles


start_funclet_proc :: Funclet f => f -> FilePath -> IO ()
start_funclet_proc f procDir = do
  -- use mVar to communicate errno
  ecVar <- newEmptyMVar
  (fh0, fh1, fh2) <- runFunclet f (putMVar ecVar)
  -- piping data between the main process and the funclet process
  void . forkIO $ bracket
    (do
        -- create main process handlers (mh)
        mh0 <- openBinaryFile (procDir </> "0.fifo") ReadMode
        mh1 <- openBinaryFile (procDir </> "1.fifo") ReadWriteMode
        mh2 <- openBinaryFile (procDir </> "2.fifo") ReadWriteMode
        mapM_ (`hSetBuffering` NoBuffering) [mh0, mh1, mh2]
        pure (mh0, mh1, mh2)
    )
    (\(mh0, mh1, mh2) -> mapM_ hClose [mh0, mh1, mh2])
    (\(mh0, mh1, mh2) -> do
        -- fork pipes
        cs <- mapM (const newEmptyMVar) [(),(),()]
        mapM_ forkIO [ binaryCat mh0 fh0 >> hClose fh0 >> putMVar (cs !! 0) ()
                     , binaryCat fh1 mh1 >> putMVar (cs !! 1) ()
                     , binaryCat fh2 mh2 >> putMVar (cs !! 2) ()
                     ]
        -- wait for all pipes to finish
        mapM_ takeMVar cs
    )
  -- wait for the errno and save it for the main process
  void . forkIO $ takeMVar ecVar >>= \ec -> do
    case ec of
      ExitSuccess       -> pure ()
      ExitFailure errno -> withFile (procDir </> "errno") WriteMode (`hPutStr` (show errno))

gen_init_code :: Int -> String
gen_init_code nFunclets = [str|\
#set -x
__initFunclets() {
  n="$1"
  d=$(dirname "$0")

  # nothing to initialize when there is no funclet
  [ "$n" == "-1" ] && return

  # create control fifos
  mkfifo "$d"/c{w,r}.fifo
  tail -f "$d"/cw.fifo > "$d"/cr.fifo &

  # create funclet fifos
  seq 0 "$n" | while read i; do
    mkdir -p "$d/$i"
    mkfifo "$d/$i"/{0,1,2}.fifo
  done
}
|] <> [fmt|__initFunclets {show (nFunclets - 1)}
|]

gen_stub_code :: String
gen_stub_code = [str|\
#+BEGIN_SRC THSH script stub code
__pipeFunclet() (
  # connect to the fifos of nth functlet
  i="$1"
  d="$(dirname "$0")/$i"

  cat <&0 > "$d"/0.fifo & pid0=$!
  cat >&1 < "$d"/1.fifo & pid1=$!
  cat >&2 < "$d"/2.fifo & pid2=$!
  trap 'kill $pid0 $pid1 $pid2' SIGINT

  echo "s $i" > "$(dirname "$0")"/cw.fifo

  wait $pid0 $pid1 $pid2
  : __pipeFunclet ended
)
#+END_SRC
|]
