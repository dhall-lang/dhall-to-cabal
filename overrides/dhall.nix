{ mkDerivation, aeson, aeson-pretty, ansi-terminal, base
, bytestring, case-insensitive, cborg, cborg-json, containers
, contravariant, cryptonite, data-fix, deepseq, Diff, directory
, doctest, dotgen, either, exceptions, filepath, foldl, gauge
, generic-random, haskeline, http-client, http-client-tls
, http-types, lens-family-core, megaparsec, memory, mockery, mtl
, network-uri, optparse-applicative, parsers, prettyprinter
, prettyprinter-ansi-terminal, profunctors, QuickCheck
, quickcheck-instances, repline, scientific, semigroups, serialise
, spoon, stdenv, tasty, tasty-expected-failure, tasty-hunit
, tasty-quickcheck, template-haskell, text, th-lift-instances
, transformers, transformers-compat, turtle, unordered-containers
, uri-encode, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.26.0";
  sha256 = "f2d0b4a1e274fbc5684922be49c43333b063880db57458e78de2bebd9afb68ca";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal base bytestring case-insensitive
    cborg cborg-json containers contravariant cryptonite data-fix
    deepseq Diff directory dotgen either exceptions filepath haskeline
    http-client http-client-tls http-types lens-family-core megaparsec
    memory mtl network-uri optparse-applicative parsers prettyprinter
    prettyprinter-ansi-terminal profunctors repline scientific
    serialise template-haskell text th-lift-instances transformers
    transformers-compat unordered-containers uri-encode vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring cborg containers data-fix deepseq directory doctest
    filepath foldl generic-random lens-family-core megaparsec mockery
    prettyprinter QuickCheck quickcheck-instances scientific semigroups
    serialise spoon tasty tasty-expected-failure tasty-hunit
    tasty-quickcheck text transformers turtle vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers directory gauge serialise text
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
