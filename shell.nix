{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

(nixpkgs.haskell.packages.${compiler}.callCabal2nix "dhall-to-cabal" ./. {}).env
