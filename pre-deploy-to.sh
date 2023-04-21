#!/usr/bin/env bash

nix copy --to ssh://$1 .#playground-hs-full
