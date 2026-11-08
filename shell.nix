let
  pkgs = import <nixpkgs> {};
  purescript-overlay = import (fetchTarball https://github.com/thomashoneyman/purescript-overlay/archive/8046182d2b23144cbb4370dabc5cc7b632bf7c02.tar.gz);
in pkgs.stdenv.mkDerivation {
  name = "purescript-unordered-collections-shell";
  buildInputs = [
    purescript-overlay.packages.${pkgs.system}.spago
    purescript-overlay.packages.${pkgs.system}.purs
    pkgs.git
    pkgs.gnumake
    pkgs.nodejs
  ];
}
