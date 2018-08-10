{ mkDerivation, cabal-install, ghc }:
dir:
let
  src-tree = mkDerivation {
    name = "src-sdist";
    buildCommand = ''
      mkdir -p $out
      cp -R ${dir}/* .
      mkdir -p dist
      chmod a+w dist
      HOME=$(pwd) cabal sdist --output-directory=$out/src
    '';
    buildInputs = [ cabal-install ghc ];
  };

in
import
  ( mkDerivation {
      name = "src.nix";
      buildCommand = ''echo "${src-tree}"/src > $out'';
    }
  )
