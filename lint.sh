#!/bin/bash

echo "hlint src/"
hlint `find ./src -name "*.hs"`

echo "hlint tests/"
hlint `find ./tests -name "*.hs"`