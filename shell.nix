{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

((nixpkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    dhall =
      let src = nixpkgs.fetchurl {
                  url = https://hackage.haskell.org/package/dhall-1.9.0/dhall-1.9.0.tar.gz;
                  sha256 = "1i59rvjjkqikz964kzrnbxfwjhwvnwqwvaw9m03yq540qi4kmiaz";
                };
           extracted = nixpkgs.runCommand "unpack-src" { buildInputs = [ nixpkgs.gnutar ]; }
             ''
               mkdir $out
               cd $out
               tar xzf ${src}
             '';
      in super.callCabal2nix "dhall" "${extracted}/dhall-1.9.0" {};
  };
}).callCabal2nix "dhall-to-cabal" ./. {}).env
