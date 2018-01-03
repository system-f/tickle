{ mkDerivation, base, bifunctors, bytestring, directory, doctest
, filepath, lens, mtl, papa, QuickCheck, semigroupoids, semigroups
, stdenv, template-haskell, transformers, validation
}:
mkDerivation {
  pname = "tickle";
  version = "0.0.6";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors bytestring filepath lens mtl papa semigroupoids
    semigroups transformers validation
  ];
  testHaskellDepends = [
    base directory doctest filepath QuickCheck template-haskell
  ];
  homepage = "https://github.com/qfpl/tickle";
  description = "A port of @Data.Binary@";
  license = stdenv.lib.licenses.bsd3;
}
