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

          dhall = pkgs.haskell.lib.dontCheck (super.callPackage ./dhall.nix {});

          repline = super.callPackage ./repline.nix {};

          formatting = super.callPackage ./formatting.nix {};

          megaparsec = super.callPackage ./megaparsec.nix {};

          parser-combinators = super.callPackage ./parser-combinators.nix {};

          tasty-hunit = super.callPackage ./tasty-hunit.nix {};

          serialise = pkgs.haskell.lib.dontCheck super.serialise;

          Cabal = self.callPackage ./cabal.nix {};

          lens = self.callPackage ./lens.nix {};

          base-compat-batteries = self.callPackage ./base-compat-batteries.nix {};

          cabal-doctest = self.callPackage ./cabal-doctest.nix { Cabal = self.callPackage ./cabal.nix {}; };

          insert-ordered-containers = self.callPackage ./insert-ordered-containers.nix {};

          QuickCheck = self.callPackage ./QuickCheck.nix {};

          quickcheck-instances = self.callPackage ./quickcheck-instances.nix {};

          splitmix = self.callPackage ./splitmix.nix {};

          transformers-compat = self.callPackage ./transformers-compat.nix {};

          dhall-to-cabal = self.callPackage ./dhall-to-cabal.nix {};
        };
      };
    };
  };

  pkgs =
    import nixpkgs { inherit config; };

in
  { inherit (pkgs.haskellPackages) dhall-to-cabal;
  }
