{ mkDerivation, base, bifunctors, doctest, hspec
, hspec-expectations-lens, lens, QuickCheck, semigroupoids
, semigroups, stdenv
}:
mkDerivation {
  pname = "meep";
  version = "0.1.2.2";
  sha256 = "d545b5c0d1688c41c1a5401ac369e14ed5bb5fff427e52a28869a9257b399aa0";
  libraryHaskellDepends = [
    base bifunctors lens semigroupoids semigroups
  ];
  testHaskellDepends = [
    base bifunctors doctest hspec hspec-expectations-lens lens
    QuickCheck semigroupoids semigroups
  ];
  description = "A silly container";
  license = stdenv.lib.licenses.bsd2;
}
