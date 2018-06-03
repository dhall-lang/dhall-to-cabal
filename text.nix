{ mkDerivation, array, base, binary, bytestring, deepseq, directory
, ghc-prim, HUnit, integer-gmp, QuickCheck, quickcheck-unicode
, random, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "text";
  version = "1.2.3.0";
  sha256 = "20e0b1627f613b32cc7f2d2e8dcc48a4a61938b24f3d14fb77cee694f0c9311a";
  libraryHaskellDepends = [
    array base binary bytestring deepseq ghc-prim integer-gmp
  ];
  testHaskellDepends = [
    array base binary bytestring deepseq directory ghc-prim HUnit
    integer-gmp QuickCheck quickcheck-unicode random test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  doCheck = false;
  homepage = "https://github.com/haskell/text";
  description = "An efficient packed Unicode text type";
  license = stdenv.lib.licenses.bsd2;
}
