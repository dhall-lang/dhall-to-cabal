{ mkDerivation, aeson, aeson-pretty, ansi-terminal, base
, bytestring, case-insensitive, cborg, cborg-json, containers
, contravariant, criterion, cryptonite, deepseq, Diff, directory
, doctest, dotgen, exceptions, filepath, foldl, haskeline
, http-client, http-client-tls, http-types, lens-family-core
, megaparsec, memory, mockery, mtl, optparse-applicative, parsers
, prettyprinter, prettyprinter-ansi-terminal, QuickCheck
, quickcheck-instances, repline, scientific, serialise, stdenv
, tasty, tasty-hunit, tasty-quickcheck, template-haskell, text
, transformers, transformers-compat, turtle, unordered-containers
, uri-encode, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.23.0";
  sha256 = "eda7b9d1baad8214f83aaf7e7ce5e374c32a62f58ca69734024fb3f254bc9d1c";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal base bytestring case-insensitive
    cborg cborg-json containers contravariant cryptonite Diff directory
    dotgen exceptions filepath haskeline http-client http-client-tls
    http-types lens-family-core megaparsec memory mtl
    optparse-applicative parsers prettyprinter
    prettyprinter-ansi-terminal repline scientific serialise
    template-haskell text transformers transformers-compat
    unordered-containers uri-encode vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring cborg containers deepseq directory doctest filepath
    foldl mockery prettyprinter QuickCheck quickcheck-instances
    serialise tasty tasty-hunit tasty-quickcheck text transformers
    turtle vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion directory serialise text
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
