{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Data.Char   (toLower)
import           System.Exit (ExitCode)
import           THSH


testSingleShellScript :: IO ExitCode
testSingleShellScript = do
  let saluteTo = "nix"
  runFuncletWithStdHandles [thsh|\
set -x
cat "$(dirname "$0")"/init.sh
cat "$0"
echo "Hello « saluteTo », 1 + 1 = « (1 :: Integer) + 1 :>4.2f»"
echo "««"
echo "»»"
|]

testScriptFunclet :: IO ExitCode
testScriptFunclet = do
  let s0 = [thsh| sed "s/Haskell/Haskell❤️/g" |]
      s1 = [thsh| echo Brrrr |]
      s2 = [thsh| bc |]
  runFuncletWithStdHandles [thsh|\
echo "Hello, Haskell." | «sh s0»
echo "" | «sh s1» | sed 's/r/R/g'
echo "Now do some maths."
for i in `seq 0 10`;do
  expr="2 ^ $i"
  echo -n "$expr = "
  echo $expr | «sh s2»
done
|]

testFn :: IO ExitCode
testFn = runFuncletWithStdHandles [thsh|\
curl -s https://example.org/ | «fn (ContentFn (\content -> "Number of occurrences of the word 'example' is "
    <> show (length (filter ((== "examples"). fmap toLower) . words $ content))
    <> "\n"
))»
: end curl

# pseudo sales numbers processing
echo -n "Sum of the sales: "; {
«fn lsum» <<EOF
("apple",  1.2, 100.2)
("orange", 2.0, 34.2)
EOF
} | tail -n1
|] where lsum = LineReadFn
                (\ (_ :: String, price, quantity) s -> let s' = s + price * quantity
                                                       in (s', Just (show s')))
                (0.0 :: Float)

main :: IO ()
main = do
  putStrLn "== testSingleShellScript"
  _ <- testSingleShellScript

  putStrLn "== testScriptFunclet"
  _ <- testScriptFunclet

  putStrLn "== testFn"
  _ <- testFn

  pure ()
