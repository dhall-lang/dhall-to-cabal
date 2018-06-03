{ mkDerivation, array, base, base-compat, base-orphans, binary
, bytestring, containers, deepseq, Diff, directory, filepath
, integer-logarithms, mtl, optparse-applicative, parsec, pretty
, process, QuickCheck, stdenv, tagged, tar, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, text, time, transformers
, tree-diff, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "2.2.0.1";
  sha256 = "02b5301304df73cea3c7d544b5026b228141dc3ac1d5b08c9a206f99aa330a7b";
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    mtl parsec pretty process text time transformers unix
  ];
  testHaskellDepends = [
    array base base-compat base-orphans bytestring containers deepseq
    Diff directory filepath integer-logarithms optparse-applicative
    pretty process QuickCheck tagged tar tasty tasty-golden tasty-hunit
    tasty-quickcheck text tree-diff
  ];
  doCheck = false;
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = stdenv.lib.licenses.bsd3;
}
