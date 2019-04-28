{ mkDerivation, array, base, base-compat, bytestring
, case-insensitive, containers, hashable, old-time, QuickCheck
, scientific, splitmix, stdenv, tagged, text, time, transformers
, transformers-compat, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "quickcheck-instances";
  version = "0.3.20";
  sha256 = "caef10da96291bf631d1437c7203237eef75206d703ad391f6e0fd07c1c9b5b8";
  libraryHaskellDepends = [
    array base base-compat bytestring case-insensitive containers
    hashable old-time QuickCheck scientific splitmix tagged text time
    transformers transformers-compat unordered-containers uuid-types
    vector
  ];
  testHaskellDepends = [
    base containers QuickCheck tagged uuid-types
  ];
  benchmarkHaskellDepends = [ base bytestring QuickCheck ];
  homepage = "https://github.com/phadej/qc-instances";
  description = "Common quickcheck instances";
  license = stdenv.lib.licenses.bsd3;
}
