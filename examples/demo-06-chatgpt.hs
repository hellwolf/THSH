#!/usr/bin/env thsh

__main__ = [thsh|\
while echo -n "chatgpt> "; do
  read q
  echo "$q" | chatgpt
done
|]

