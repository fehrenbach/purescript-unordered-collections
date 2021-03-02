let
  pkgs = import <nixpkgs> {};
  easy-ps = import (fetchTarball https://github.com/justinwoo/easy-purescript-nix/archive/3b4039475c245243716b1e922455a9062c0531da.tar.gz) { inherit pkgs; };
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
