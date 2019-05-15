{ mkDerivation, array, base, base-compat, bytestring
, case-insensitive, containers, hashable, old-time, QuickCheck
, scientific, stdenv, tagged, text, time, transformers
, transformers-compat, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "quickcheck-instances";
  version = "0.3.16.1";
  sha256 = "7fb107e2bbaa58403aaf96a6fc84672623cf94c52b314fd69e70fb948f5e6507";
  libraryHaskellDepends = [
    array base base-compat bytestring case-insensitive containers
    hashable old-time QuickCheck scientific tagged text time
    transformers transformers-compat unordered-containers uuid-types
    vector
  ];
  testHaskellDepends = [
    base containers QuickCheck tagged uuid-types
  ];
  jailbreak = true;
  homepage = "https://github.com/phadej/qc-instances";
  description = "Common quickcheck instances";
  license = stdenv.lib.licenses.bsd3;
}
