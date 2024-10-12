#!/usr/bin/env thsh

import Data.Char (toLower)

__main__ = [thsh|\
curl -s https://www.haskell.org/ | «fn (ContentFn $
  \content -> "Number of occurrences of the word 'haskell' is "
    <> show (length (filter ((== "haskell"). fmap toLower) . words $ content))
    <> "\n"
  )
»
|]
