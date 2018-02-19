#!/bin/bash
set -e
########################################

cabal2nix .         > "reflex-audio-default.nix"
cabal2nix . --shell > "reflex-audio-shell.nix"

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
