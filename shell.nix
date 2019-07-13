let
  pkgs = import <nixpkgs> {};
  easy-ps = import (fetchTarball https://github.com/justinwoo/easy-purescript-nix/archive/9a8d138663c5d751e3a84f1345166e1f0f760a07.tar.gz) { inherit pkgs; };
in pkgs.stdenv.mkDerivation {
  name = "purescript-unordered-collections-shell";
  buildInputs = [
    pkgs.nodePackages.bower
    pkgs.git # required by bower for github dependencies
    pkgs.gnumake
    easy-ps.purs
  ];
}
