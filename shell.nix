let
  pkgs = import <nixpkgs> {};
  easy-ps = import (fetchTarball https://github.com/justinwoo/easy-purescript-nix/archive/0b2c378b360ca0ff92e3fe3a41d191457036b4d6.tar.gz);
in pkgs.stdenv.mkDerivation {
  name = "purescript-unordered-collections-shell";
  buildInputs = [
    pkgs.nodePackages.bower
    pkgs.git # required by bower for github dependencies
    pkgs.gnumake
    easy-ps.purescript
  ];
}
