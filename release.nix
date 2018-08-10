let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev = "804060ff9a79ceb0925fe9ef79ddbf564a225d47";

    sha256 = "01pb6p07xawi60kshsxxq1bzn8a0y4s5jjqvhkwps4f5xjmmwav3";

    outputSha256 = "0ga345hgw6v2kzyhvf5kw96hf60mx5pbd9c4qj5q4nan4lr7nkxn";
  };

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          text = self.callPackage ./text.nix {};

          prettyprinter = self.callPackage ./prettyprinter.nix {};

          dhall = super.callPackage ./dhall.nix {};

          repline = pkgs.haskell.lib.doJailbreak super.repline;

          formatting = super.callPackage ./formatting.nix {};

          dhall-to-cabal =
            super.callCabal2nix
              "dhall-to-cabal"
              ( import
                  ./cabal-sdist.nix
                  { inherit ( pkgs.stdenv ) mkDerivation;
                    inherit ( pkgs ) cabal-install;
                    ghc = self.ghcWithPackages (hs: [
                      (hs.callPackage ./cabal.nix {})
                    ]);
                  }
                  ./.
              )
              {
                Cabal = self.callPackage ./cabal.nix {};
              };
        };
      };
    };
  };

  pkgs =
    import nixpkgs { inherit config; };

in
  { inherit (pkgs.haskellPackages) dhall-to-cabal;
  }
