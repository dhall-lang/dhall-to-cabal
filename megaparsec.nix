{ mkDerivation, base, bytestring, case-insensitive, containers
, criterion, deepseq, hspec, hspec-expectations, mtl
, parser-combinators, QuickCheck, scientific, stdenv, text
, transformers, weigh
}:
mkDerivation {
  pname = "megaparsec";
  version = "7.0.1";
  sha256 = "6bbb385ea8bfb5b112dca234257bed36c061c56fbb576edda1b858549a7fc02c";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  testHaskellDepends = [
    base bytestring case-insensitive containers hspec
    hspec-expectations mtl parser-combinators QuickCheck scientific
    text transformers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq text weigh
  ];
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd2;
}
