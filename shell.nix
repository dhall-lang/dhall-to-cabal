{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

((nixpkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    prettyprinter = self.callPackage ./prettyprinter.nix {};

    dhall = super.callPackage ./dhall.nix {};
    
    repline = nixpkgs.haskell.lib.doJailbreak super.repline;
    
    formatting = super.callPackage ./formatting.nix {};
  };
}).callCabal2nix "dhall-to-cabal" ./. {}).env
