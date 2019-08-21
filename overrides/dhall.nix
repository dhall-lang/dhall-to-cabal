{ mkDerivation, aeson, aeson-pretty, ansi-terminal, base
, bytestring, case-insensitive, cborg, cborg-json, containers
, contravariant, cryptonite, deepseq, Diff, directory, doctest
, dotgen, either, exceptions, filepath, foldl, gauge, haskeline
, http-client, http-client-tls, http-types, lens-family-core
, megaparsec, memory, mockery, mtl, optparse-applicative, parsers
, prettyprinter, prettyprinter-ansi-terminal, profunctors
, QuickCheck, quickcheck-instances, repline, scientific, semigroups
, serialise, spoon, stdenv, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text, th-lift-instances, transformers
, transformers-compat, turtle, unordered-containers, uri-encode
, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.25.0";
  sha256 = "10604f648af61bd5ea784769cfe2ff1518a32e7e4e22615e9cbe6fae1ce91835";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal base bytestring case-insensitive
    cborg cborg-json containers contravariant cryptonite deepseq Diff
    directory dotgen either exceptions filepath haskeline http-client
    http-client-tls http-types lens-family-core megaparsec memory mtl
    optparse-applicative parsers prettyprinter
    prettyprinter-ansi-terminal profunctors repline scientific
    serialise template-haskell text th-lift-instances transformers
    transformers-compat unordered-containers uri-encode vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring cborg containers deepseq directory doctest filepath
    foldl lens-family-core megaparsec mockery prettyprinter QuickCheck
    quickcheck-instances semigroups serialise spoon tasty tasty-hunit
    tasty-quickcheck text transformers turtle vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers directory gauge serialise text
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
