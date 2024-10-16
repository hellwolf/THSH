{-|
Module      : THSH.Fn
Description : Fn funclets are Haskell functions that talk with other funclets including the main shell script.
Copyright   : (c) Miao ZhiCheng, 2024
License     : MIT
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : POSIX
-}
module THSH.Fn
  ( FnFunction (..)
  , ContentFn (..), stringContentFn, stringContentIOFn, textContentFn, textContentIOFn
  , LineReadFn (..), lineReadFn
  , Fn, fn ) where

import           System.Exit  (ExitCode (..))
import           System.IO    (Handle, hGetContents, hGetLine, hIsEOF, hPutStr)
-- text
import qualified Data.Text    as T
import qualified Data.Text.IO
--
import           THSH.Funclet (Funclet (..))


-- | A 'FnFunction' is a function that, given a set of handles to communicate with it, it returns an exit code.
class FnFunction f where
  runFn :: f -> (Handle, Handle, Handle) -> IO ExitCode

-- | The new type wrapper of any "FnFunction" instance.
newtype Fn f = MkFn f

-- | The marker for the 'thsh' quasi-quote to recognize a 'FnFunction' code block.
fn :: FnFunction f => f -> Fn f
fn = MkFn

-- | A 'FnFunction' that converts the entire input content to another as 'String'.
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

-- | 'ContentFn' for the 'Data.Text' type from the text package.
textContentFn :: (T.Text -> T.Text) -> ContentFn IO T.Text
textContentFn f = MkContentFn (pure . f) Data.Text.IO.hGetContents Data.Text.IO.hPutStr

-- | IO variant of 'textContentFn'.
textContentIOFn :: (T.Text -> IO T.Text) -> ContentFn IO T.Text
textContentIOFn f = MkContentFn f Data.Text.IO.hGetContents Data.Text.IO.hPutStr

-- | A 'FnFunction' that reads line by line via 'Read' instances of @a@ and accumulates context @b@.
data LineReadFn m a b = Read a
                      => MkLineReadFn
                         (a -> b -> m (b, Maybe String)) -- ^ read an element; accumulate context; and maybe an output
                         (b -> m (Maybe String))         -- ^ final output
                         b                               -- ^ initial context

-- | Idiomatic wrapper for the `MkLineReadFn`
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

-- | The 'FnFunction' instance of 'Funclet'.
instance FnFunction f => Funclet (Fn f) where
  runFuncletWithHandles (MkFn f) cb handles = cb =<< runFn f handles
