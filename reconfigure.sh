#!/bin/bash
set -e
########################################

cabal2nix . > "nix/reflex-audio.nix"

./provision.sh

########################################
#
# SHELL_FILE=shell-reflex-audio.nix
# cabal2nix . --shell > "$SHELL_FILE"
#
# if [ ! -f "$SHELL_FILE" ]; then 
#   # don't overwrite if it already exists
#   cabal2nix . --shell > "$SHELL_FILE" 
# fi
#
