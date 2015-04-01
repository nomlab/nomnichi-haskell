#!/bin/sh

if [ ! -d .cabal-sandbox ]; then
  cabal sandbox init
fi
cabal install --max-backjumps=-1 --only-dependencies 
cabal build
