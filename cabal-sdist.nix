{ mkDerivation, cabal-install }:
dir:
let
  src-tree = mkDerivation {
    name = "src-sdist";
    buildCommand = ''
      mkdir -p $out/src
      cd ${dir}
      HOME=$(pwd) cabal sdist --output-directory=$out/src
    '';
    buildInputs = [ cabal-install ];
  };

in
import
  ( mkDerivation {
      name = "src.nix";
      buildCommand = ''echo "${src-tree}"/src > $out'';
    }
  )
