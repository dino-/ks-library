#! /bin/bash

set -e

sandboxDir="../cabal-sandbox"

cabal sandbox init --sandbox=$sandboxDir
cabal install
