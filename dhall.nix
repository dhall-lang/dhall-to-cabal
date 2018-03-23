{ mkDerivation, ansi-terminal, ansi-wl-pprint, base
, base16-bytestring, bytestring, case-insensitive, containers
, contravariant, cryptonite, deepseq, directory, exceptions
, fetchgit, filepath, formatting, haskeline, http-client
, http-client-tls, insert-ordered-containers, lens-family-core
, memory, mtl, optparse-generic, parsers, prettyprinter
, prettyprinter-ansi-terminal, repline, scientific, stdenv, tasty
, tasty-hunit, text, transformers, trifecta, unordered-containers
, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.11.1";
  src = fetchgit {
    url = "git://github.com/dhall-lang/dhall-haskell";
    sha256 = "04jxiy61bmwpr84y1xs3dxbwnmsf1k7ycmga41p32m3yrsl0dqc7";
    rev = "ed2041ee7709969ca63b4d9d1a82a34ffc145f3d";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base base16-bytestring bytestring case-insensitive
    containers contravariant cryptonite directory exceptions filepath
    formatting http-client http-client-tls insert-ordered-containers
    lens-family-core memory parsers prettyprinter
    prettyprinter-ansi-terminal scientific text transformers trifecta
    unordered-containers vector
  ];
  executableHaskellDepends = [
    ansi-terminal base haskeline mtl optparse-generic prettyprinter
    prettyprinter-ansi-terminal repline text trifecta
  ];
  testHaskellDepends = [
    base deepseq insert-ordered-containers prettyprinter tasty
    tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
