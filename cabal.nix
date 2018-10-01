{ mkDerivation, array, base, base-compat, base-orphans, binary
, bytestring, containers, deepseq, Diff, directory, filepath
, integer-logarithms, mtl, optparse-applicative, parsec, pretty
, process, QuickCheck, stdenv, tagged, tar, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, temporary, text, time
, transformers, tree-diff, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "2.4.0.1";
  sha256 = "3b9bc0aa883576d80e5ab2671ffd6d05e7a1c04497ad74331feb7eaf1e4d3498";
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    mtl parsec pretty process text time transformers unix
  ];
  testHaskellDepends = [
    array base base-compat base-orphans bytestring containers deepseq
    Diff directory filepath integer-logarithms optparse-applicative
    pretty process QuickCheck tagged tar tasty tasty-golden tasty-hunit
    tasty-quickcheck temporary text tree-diff
  ];
  doCheck = false;
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = stdenv.lib.licenses.bsd3;
}
