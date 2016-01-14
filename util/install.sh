#! /bin/bash

set -e

sandboxDir="../cabal-sandbox"

cabal clean
cabal sandbox init --sandbox=$sandboxDir
cabal install
