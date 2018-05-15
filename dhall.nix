{ mkDerivation, ansi-terminal, base, bytestring, case-insensitive
, containers, contravariant, cryptonite, deepseq, directory
, exceptions, filepath, formatting, haskeline, http-client
, http-client-tls, insert-ordered-containers, lens-family-core
, megaparsec, memory, mtl, optparse-applicative, parsers
, prettyprinter, prettyprinter-ansi-terminal, repline, scientific
, stdenv, tasty, tasty-hunit, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.13.1";
  sha256 = "c075ac87c2fe5f47573b872126dbb94cabc4441fb532135963741d7edbec50d6";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring case-insensitive containers
    contravariant cryptonite directory exceptions filepath formatting
    http-client http-client-tls insert-ordered-containers
    lens-family-core megaparsec memory parsers prettyprinter
    prettyprinter-ansi-terminal scientific text transformers
    unordered-containers vector
  ];
  executableHaskellDepends = [
    ansi-terminal base haskeline megaparsec mtl optparse-applicative
    prettyprinter prettyprinter-ansi-terminal repline text
  ];
  testHaskellDepends = [
    base deepseq insert-ordered-containers prettyprinter tasty
    tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
