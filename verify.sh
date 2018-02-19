#!/bin/sh
set -e
./configure.sh
./build.sh

./test.sh     || true
./document.sh || true
./bench.sh    || true
./run.sh      || true

# cabal test    || true
# cabal bench   || true
# cabal haddock || true
