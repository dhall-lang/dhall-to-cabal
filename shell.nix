{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

((nixpkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    prettyprinter = self.callPackage ./prettyprinter.nix {};

    dhall =
      super.callPackage
        ( nixpkgs.fetchFromGitHub {
            repo = "dhall-haskell";
            owner = "dhall-lang";
            rev = "404c97d96c4ee00db773bd3d23fdcf7e93b9fd23";
            sha256 = "1v584yqy1k8p1bjyzzvd0x1pqahzwgac7mmpbmapjlsqvrrjk43q";
          }
        )
        {};
  };
}).callCabal2nix "dhall-to-cabal" ./. {}).env
