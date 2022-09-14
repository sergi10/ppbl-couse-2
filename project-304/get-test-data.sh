#!/bin/sh
git clone git@github.com:ekmett/semigroupoids
cd semigroupoids
cabal clean
cabal build --ghc-options=-fwrite-ide-info



