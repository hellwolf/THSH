#!/usr/bin/env thsh

import Data.Char (toLower)

__main__ = [thsh|\
curl -s https://www.haskell.org/ | «fn (stringContentFn $
  \content -> "Number of occurrences of the word 'haskell' on haskell.org is "
    <> show (length (filter ((== "haskell"). fmap toLower) . words $ content))
    <> "\n"
  )
»
|]
