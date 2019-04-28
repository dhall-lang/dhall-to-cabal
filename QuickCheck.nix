{ mkDerivation, base, containers, deepseq, process, random
, splitmix, stdenv, template-haskell, transformers
}:
mkDerivation {
  pname = "QuickCheck";
  version = "2.13.1";
  sha256 = "a6463a96e7af7d6d5614dc919adca6e740b92a66334a55333ffbe44cac89d9c6";
  libraryHaskellDepends = [
    base containers deepseq random splitmix template-haskell
    transformers
  ];
  testHaskellDepends = [ base deepseq process ];
  homepage = "https://github.com/nick8325/quickcheck";
  description = "Automatic testing of Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
