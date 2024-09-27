module THSH.Fn (fn) where

import           GHC.IO.StdHandles (stderr)
--
import           System.Exit       (ExitCode (..))
import           System.IO         (hClose, hGetContents, hGetLine, hPutStr, hPutStrLn)
--
import           THSH.Funclet      (Funclet (..))


data Fn a = Fn a

instance Funclet (Fn (String -> String)) where
  runFunclet (Fn f) (hIn, hOut, hErr) = do
    !content <- hGetContents hIn
    hPutStr hOut (f content)
    pure ExitSuccess

-- instance Funclet ([String] -> [String])

fn :: Funclet (Fn a) => a -> Fn a
fn = Fn
