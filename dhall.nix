{ mkDerivation, ansi-terminal, base, bytestring, case-insensitive
, cborg, containers, contravariant, criterion, cryptonite, deepseq
, Diff, directory, doctest, exceptions, filepath, haskeline
, http-client, http-client-tls, lens-family-core, megaparsec
, memory, mockery, mtl, optparse-applicative, parsers
, prettyprinter, prettyprinter-ansi-terminal, QuickCheck
, quickcheck-instances, repline, scientific, serialise, stdenv
, tasty, tasty-hunit, tasty-quickcheck, template-haskell, text
, transformers, unordered-containers, uri-encode, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.19.0";
  sha256 = "ff111c69d347bc77f64186a3a9f56243a01fc06d7d8a3476734e02ba0727e2eb";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring case-insensitive cborg containers
    contravariant cryptonite Diff directory exceptions filepath
    haskeline http-client http-client-tls lens-family-core megaparsec
    memory mtl optparse-applicative parsers prettyprinter
    prettyprinter-ansi-terminal repline scientific serialise
    template-haskell text transformers unordered-containers uri-encode
    vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers deepseq directory doctest filepath mockery
    prettyprinter QuickCheck quickcheck-instances serialise tasty
    tasty-hunit tasty-quickcheck text transformers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion directory serialise text
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
