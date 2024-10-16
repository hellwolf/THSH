#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -rf "$dir"' EXIT

# assumes cabal 2.4 or later
cabal haddock --builddir="$dir" --haddock-for-hackage --enable-doc

cabal upload -d --publish $dir/*-docs.tar.gz
