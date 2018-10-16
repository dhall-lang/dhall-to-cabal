{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "parser-combinators";
  version = "1.0.0";
  sha256 = "e54c8d6071bc67866dffb661e5f56de6d632f40abdfe76b9f56a734ca76e8edf";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/mrkkrp/parser-combinators";
  description = "Lightweight package providing commonly useful parser combinators";
  license = stdenv.lib.licenses.bsd3;
}
