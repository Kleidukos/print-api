#!/usr/bin/env bash

for version in $(ghcup list --raw-format --tool ghc --show-criteria installed | cut -d ' ' -f 2); do
    cabal install "print-api:exe:print-api-${version}" --with-compiler "ghc-${version}"
done
cabal install "print-api:exe:print-api"
