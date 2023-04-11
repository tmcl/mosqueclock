{ mkDerivation, aeson, base, stdenv, text, time, timezone-series, lib, hedgehog
, hspec, timezone-olson, with-utf8, typed-process, stm, pkgs }:
mkDerivation {
  pname = "athan-calculator";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base text time timezone-series ];
  testHaskellDepends = [ base hedgehog hspec timezone-olson ];
  executableHaskellDepends = if true /* system == "x86_64-linux"*/ then [
    base
    hedgehog
    with-utf8
    typed-process
    stm
  ] else
    [ ];
  description = "athan time calculations";
  license = lib.licenses.bsd3;
}
