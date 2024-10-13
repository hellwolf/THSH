#!/usr/bin/env thsh

s0 :: Script
s0 = [thsh| bc |]

__main__ = [thsh|\
for i in `seq 0 10`;do
  expr="2 ^ $i"
  echo -n "$expr = "
  echo $expr | «sh s0»
done
|]
