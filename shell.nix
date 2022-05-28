let
  pkgs = import <nixpkgs> {};
  easy-ps = import (fetchTarball https://github.com/justinwoo/easy-purescript-nix/archive/cbcb53725c430de4e69f652d69c1677e17c6bcec.tar.gz) { inherit pkgs; };
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
