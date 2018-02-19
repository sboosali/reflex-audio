#!/bin/sh
nix-shell --run 'cabal configure --enable-tests --enable-benchmarks'
