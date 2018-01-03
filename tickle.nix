{ mkDerivation, base, bifunctors, bytestring, checkers, filepath
, lens, mtl, papa, QuickCheck, semigroupoids, semigroups, stdenv
, tasty, tasty-hunit, tasty-quickcheck, transformers, validation
}:
mkDerivation {
  pname = "tickle";
  version = "0.0.9";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors bytestring filepath mtl papa semigroupoids
    semigroups transformers validation
  ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/tickle";
  description = "A port of @Data.Binary@";
  license = stdenv.lib.licenses.bsd3;
}
