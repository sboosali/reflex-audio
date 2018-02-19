{ mkDerivation, ALUT, base, bytestring, containers, deepseq
, doctest, exceptions, hashable, hedgehog, lens, mtl, OpenAL
, QuickCheck, spiros, StateVar, stdenv, tasty, tasty-hedgehog
, tasty-quickcheck, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "reflex-audio";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ALUT base bytestring containers deepseq exceptions hashable lens
    mtl OpenAL spiros StateVar text transformers unordered-containers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest hedgehog QuickCheck tasty tasty-hedgehog
    tasty-quickcheck
  ];
  homepage = "http://github.com/sboosali/reflex-audio#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
