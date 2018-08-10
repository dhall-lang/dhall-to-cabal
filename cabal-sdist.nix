pkgs: dir:
let
  src-tree = pkgs.stdenv.mkDerivation {
    name = "src-sdist";
    buildCommand = ''
      mkdir -p $out/src
      cd ${dir}
      cabal sdist --output-directory=$out/src
    '';
    buildInputs = [ pkgs.cabal-install ];
  };

in
import
  ( pkgs.stdenv.mkDerivation {
      name = "src.nix";
      buildCommand = ''echo "${src-tree}"/src > $out'';
    }
  )
