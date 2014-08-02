#!/bin/bash

cabal sandbox init
cabal install --dependencies-only
cabal install
cd ./spc-ssvm/
cabal sandbox init --sandbox=../.cabal-sandbox/
cabal install --dependencies-only
cabal install
cd ..
cd ./spc-jvm/
cabal sandbox init --sandbox=../.cabal-sandbox/
cabal install ../../hs-java/
cabal install --dependencies-only
cabal install


