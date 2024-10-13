module THSH.Fn
  ( ContentFn (..), stringContentFn, stringContentIOFn, textContentFn, textContentIOFn
  , LineReadFn (..), lineReadFn
  , fn ) where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (bracket)
import           System.Exit             (ExitCode (..))
import           System.IO               (BufferMode (NoBuffering), Handle, hClose, hGetContents, hGetLine, hIsEOF,
                                          hPutStr, hSetBuffering)
-- process
import           System.Process          (createPipe)
-- text
import qualified Data.Text               as T
import qualified Data.Text.IO
--
import           THSH.Funclet            (Funclet (..))


-- | A 'Fn' is a function that, given a set of handles to communicate with it, it returns an exit code.
class FnFunction f where
  runFn :: f -> (Handle, Handle, Handle) -> IO ExitCode

-- | A 'Fn' that converts the entire input content to another as 'String'.
data ContentFn m s = MkContentFn (s -> m s) (Handle -> m s) (Handle -> s -> m ())

instance FnFunction (ContentFn IO s) where
  runFn (MkContentFn f r w) (hIn, hOut, _) = do
    content <- r hIn
    w hOut =<< f content
    pure ExitSuccess

-- | 'ContentFn' for the 'String' type.
stringContentFn :: (String -> String) -> ContentFn IO String
stringContentFn f = MkContentFn (pure . f) hGetContents hPutStr

-- | IO variant of 'stringContentFn'.
stringContentIOFn :: (String -> IO String) -> ContentFn IO String
stringContentIOFn f = MkContentFn f hGetContents hPutStr

-- | 'ContentFn' for the 'Text' type from the text package.
textContentFn :: (T.Text -> T.Text) -> ContentFn IO T.Text
textContentFn f = MkContentFn (pure . f) Data.Text.IO.hGetContents Data.Text.IO.hPutStr

-- | IO variant of 'textContentFn'.
textContentIOFn :: (T.Text -> IO T.Text) -> ContentFn IO T.Text
textContentIOFn f = MkContentFn f Data.Text.IO.hGetContents Data.Text.IO.hPutStr

-- | A 'Fn' that reads line by line via 'Read' instances of @a@ and accumulates context @b@.
data LineReadFn m a b = Read a
                      => MkLineReadFn
                         (a -> b -> m (b, Maybe String)) -- read an element; accumulate context; and maybe an output
                         (b -> m (Maybe String))         -- final output
                         b                               -- initial context

-- Idiomatic wrapper for the `MkLineReadFn`
lineReadFn :: forall a b.
              Read a
           => (a -> b -> (b, Maybe String))
           -> (b -> Maybe String)
           -> b
           -> LineReadFn IO a b
lineReadFn f fin b0 = MkLineReadFn ((pure .) . f) (pure . fin) b0

instance FnFunction (LineReadFn IO a b) where
  runFn (MkLineReadFn f fin b0) (hIn, hOut, _) = do
    let go b (a:as) = a >>= \case
          Just a'  -> f a' b >>= \ (b', r) ->
            case r of
              Just r' -> hPutStr hOut r'
              Nothing -> pure ()
            >> go b' as
          Nothing -> fin b >>= \case
            Just a' -> hPutStr hOut a'
            Nothing -> pure () -- input lines finished
        go _ _ = error "impossible"
    -- repeatedly reading lines for @go@ to process, which should end with an infinite list of Nothings.
    go b0 $ repeat (hIsEOF hIn >>= \ case
                       False -> hGetLine hIn >>= pure . Just . (read :: String -> a)
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
