module THSH.Fn
  ( ContentFn (..)
  , LineReadFn (..)
  , fn ) where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (bracket)
import           System.Exit             (ExitCode (..))
import           System.IO               (BufferMode (NoBuffering), Handle, hClose, hGetContents, hGetLine, hIsEOF,
                                          hPutStr, hPutStrLn, hSetBuffering)
import           System.Process          (createPipe)
--
import           THSH.Funclet            (Funclet (..))


-- | A 'Fn' is a function that, given a set of handles to communicate with it, it returns an exit code.
class FnFunction f where
  runFn :: f -> (Handle, Handle, Handle) -> IO ExitCode

-- | A 'Fn' that converts the entire input content to another
newtype ContentFn = ContentFn (String -> String)
instance FnFunction ContentFn where
  runFn (ContentFn f) (hIn, hOut, _) = do
    !content <- hGetContents hIn
    hPutStr hOut (f content)
    pure ExitSuccess

-- | A 'Fn' that reads line by line via 'Read' instances of @a@ and accumulates context @b@.
data LineReadFn a b = Read a => LineReadFn (a -> b -> (b, Maybe String)) b
instance FnFunction (LineReadFn a b) where
  runFn (LineReadFn f b0) (hIn, hOut, _) = do
    let go b (a:as) = a >>= \case
          Just a'  -> let (b', r) = f a' b
            in case r of
                 Just r' -> hPutStrLn hOut r'
                 Nothing -> pure ()
               >> go b' as
          Nothing -> pure ()
        go _ _ = error "impossible"
    go b0 $ repeat (hIsEOF hIn >>= \ case
                       False -> pure . Just . (read :: String -> a) =<< hGetLine hIn
                       True  -> pure Nothing)
    pure ExitSuccess

-- | 'Fn' wraps a type of 'FnFunction' instance.
data Fn f = FnFunction f => Fn f

-- | 'Fn' is a trivial 'Funclet'.
instance Funclet (Fn f) where
  runFunclet (Fn f) cb = do
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
      (\(hInR, hOutW, hErrW) -> cb =<< runFn f (hInR, hOutW, hErrW))
    takeMVar handles

fn :: FnFunction f => f -> Fn f
fn = Fn
