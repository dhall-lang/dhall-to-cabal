{ mkDerivation, base, base-orphans, bifunctors, comonad
, contravariant, distributive, stdenv, tagged, transformers
}:
mkDerivation {
  pname = "profunctors";
  version = "5.4";
  sha256 = "10ba750039cf29aa985aa6b39c1b055d28f02ae0ffc6923f8da8e5d1768bb0ac";
  libraryHaskellDepends = [
    base base-orphans bifunctors comonad contravariant distributive
    tagged transformers
  ];
  homepage = "http://github.com/ekmett/profunctors/";
  description = "Profunctors";
  license = stdenv.lib.licenses.bsd3;
}
