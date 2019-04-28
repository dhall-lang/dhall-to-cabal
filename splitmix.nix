{ mkDerivation, async, base, base-compat-batteries, bytestring
, containers, criterion, deepseq, process, random, stdenv
, tf-random, time, vector
}:
mkDerivation {
  pname = "splitmix";
  version = "0.0.2";
  sha256 = "e6abb71ab9c1daeb35262523f23adf7d5d70dfbf097ec40caf91b35866a43bf9";
  libraryHaskellDepends = [ base deepseq random time ];
  testHaskellDepends = [
    async base base-compat-batteries bytestring deepseq process random
    tf-random vector
  ];
  benchmarkHaskellDepends = [
    base containers criterion random tf-random
  ];
  description = "Fast Splittable PRNG";
  license = stdenv.lib.licenses.bsd3;
}
