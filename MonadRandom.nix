{ mkDerivation, base, mtl, primitive, random, stdenv, transformers
, transformers-compat
}:
mkDerivation {
  pname = "MonadRandom";
  version = "0.5.1.1";
  sha256 = "abda4a297acf197e664695b839b4fb70f53e240f5420489dc21bcf6103958470";
  revision = "3";
  editedCabalFile = "0fiblwmwk48d1g9j99qrcg1ak904csgfb86y80d1nl2vr782cq6w";
  libraryHaskellDepends = [
    base mtl primitive random transformers transformers-compat
  ];
  description = "Random-number generation monad";
  license = stdenv.lib.licenses.bsd3;
}
