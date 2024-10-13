#!/usr/bin/env thsh
{- cabal:
build-depends: text
default-extensions: OverloadedStrings
-}
import qualified Data.Text as T

__main__ = [thsh|\
curl -s https://www.haskell.org/ | «fn (textContentFn $
  \content -> "Number of occurrences of the word 'haskell' on haskell.org is "
    <> (T.pack . show . length) (filter ((== "haskell") . T.toLower) . T.words $ content)
    <> "\n"
  )
»
|]
