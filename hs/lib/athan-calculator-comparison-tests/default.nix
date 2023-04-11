{ mkDerivation, aeson, base, stdenv, text, time, timezone-series, lib, hedgehog
, hspec, timezone-olson, with-utf8, typed-process, stm, system, pkgs
, athan-calculator, utf8-string }:
mkDerivation {
  pname = "athan-calculator";
  version = "0.1.0.0";
  src = ./.;
  executableHaskellDepends = if system == "x86_64-linux" then [
    base
    hedgehog
    hspec
    stm
    text
    time
    timezone-olson
    timezone-series
    typed-process
    with-utf8
    utf8-string
    athan-calculator
  ] else
    [ ];
  description = "athan time calculations";
  license = lib.licenses.bsd3;
}
