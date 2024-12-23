
# shellcheck shell=bash

LC_ALL := 'C.UTF-8'

default:
  @just --list

# check that the code is formatted with fourmolu
syntax:
  nix develop --command fourmolu -i .

# check that the code is properly linted
hlint:
  nix develop --command hlint .

# build the docker image
build:
  nix build .
  mkdir -p tmp
  chmod +w -R tmp
  rm -rf ./tmp/*
  # shellcheck disable=SC2046
  cp -R $(nix-store -qR result/) tmp
  cp -L ./result/bin/buildkite-notification ./tmp/buildkite-notification
  docker build . -t paolino/buildkite-notification:latest

# build after clean
clean-build:
  cabal clean
  just build
