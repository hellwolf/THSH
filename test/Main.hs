{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           THSH
import           THSH.SubProc (runSH)

main :: IO ()
main = do
  let say = "nix"
      folder = "/etc/"
  _ <- runSH [thsh|
echo "Hello % say %,"
ls "% folder %"
|]
  putStr "."
