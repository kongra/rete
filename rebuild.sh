#!/bin/bash

rm -rf .cabal-sandbox
rm -rf dist

cabal sandbox init
cabal sandbox add-source ../kask-base
cabal sandbox add-source ../treeprint

cabal install -j
