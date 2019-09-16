let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "ac827e0e35b8a3f7370ff0dbaea7387dbc514c6c";
  };

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          # Local packages
          dhall-to-cabal = self.callPackage ./dhall-to-cabal.nix {};

          # Things we need newer versions of.
          Cabal = self.callPackage ./overrides/Cabal.nix {};

          dhall = pkgs.haskell.lib.dontCheck (super.callPackage ./overrides/dhall.nix {});

          ghc-paths = self.callPackage ./overrides/ghc-paths.nix {};
        };
      };
    };
  };

  pkgs =
    import nixpkgs { inherit config; };

in
  { inherit (pkgs.haskellPackages) dhall-to-cabal;
  }
