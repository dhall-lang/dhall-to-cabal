{ mkDerivation, base, base-compat, hspec, hspec-discover
, QuickCheck, stdenv
}:
mkDerivation {
  pname = "base-compat-batteries";
  version = "0.10.5";
  sha256 = "175dcfd1453bd02ec955c05181cbf4278af145183b5899c62d3be29d866170ee";
  revision = "1";
  editedCabalFile = "15sn2qc8k0hxbb2nai341kkrci98hlhzcj2ci087m0zxcg5jcdbp";
  libraryHaskellDepends = [ base base-compat ];
  testHaskellDepends = [ base hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  description = "base-compat with extra batteries";
  license = stdenv.lib.licenses.mit;
}
