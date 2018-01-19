{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

((nixpkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    dhall =
      let src = nixpkgs.fetchFromGitHub {
                  owner = "ocharles";
                  repo = "dhall-haskell";
                  rev = "b6d7e1a1ca062cb446a9d28c0f87d6f4fe87903c";
                  sha256 = "13n8b3ya71w93rsix5i4mjbrnkrjfv022q9x2dbw8n2a4llr43pp";
                };
      in super.callCabal2nix "dhall" "${src}" {};
  };
}).callCabal2nix "dhall-to-cabal" ./. {}).env
