{ mkDerivation, base, call-stack, stdenv, tasty }:
mkDerivation {
  pname = "tasty-hunit";
  version = "0.10.0.1";
  sha256 = "8f903bef276ef503e4ef8b66a1e201c224588e426bc76f7581480f66d47b7048";
  libraryHaskellDepends = [ base call-stack tasty ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "HUnit support for the Tasty test framework";
  license = stdenv.lib.licenses.mit;
}
