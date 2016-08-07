#!/bin/bash

cabal build --ghc-options="-W -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-signatures -Widentities -fforce-recomp -fno-code"
