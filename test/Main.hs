{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           System.Exit (ExitCode)
import           THSH


testSingleShellScript :: IO ExitCode
testSingleShellScript = do
  let say = "nix"
      folder = "/etc/"
      s0 = [thsh| echo "Hello, I am a funclet." |]
  runFuncletWithStdHandles [thsh|\
set -x
# cat "$(dirname "$0")"/init.sh
# cat "$0"
echo "Hello % say !, 1 + 1 = % (1 :: Integer) + 1 :>4.2f!"
ls "% folder !"
echo "Multiline %
say <> " is good"
! Escaped"
echo "%%"
echo "!!"
%sh s0!
|]

testScriptFunclet :: IO ExitCode
testScriptFunclet = do
  let s0 = [thsh| sed "s/Haskell/Haskell❤️/g" |]
      s1 = [thsh| echo Brrrr |]
  runFuncletWithStdHandles [thsh|\
echo "Hello, Haskell." | %sh s0!
%sh s1! | sed 's/r/R/g'
# for i in `seq 0 10`;do
#   expr="2 ^ $i"
#   echo -n "$expr = "
#   echo $expr | __pipeFunclet 2
# done
|]

main :: IO ()
main = do
  putStrLn "== testSingleShellScript"
  _ <- testSingleShellScript
  putStrLn "== testScriptFunclet"
  _ <- testScriptFunclet
  pure ()
