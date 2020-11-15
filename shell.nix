let
  pkgs = import <nixpkgs> {};
  easy-ps = import (fetchTarball https://github.com/justinwoo/easy-purescript-nix/archive/7b1c1635e16c7f12065db2f8ec049030fcc63655.tar.gz) { inherit pkgs; };
in pkgs.stdenv.mkDerivation {
  name = "purescript-unordered-collections-shell";
  buildInputs = [
    easy-ps.pulp
    easy-ps.purs
    pkgs.git # required by bower for github dependencies
    pkgs.gnumake
    pkgs.nodePackages.bower
  ];
}
