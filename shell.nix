let
  pkgs = import <nixpkgs> {};
  easy-ps = import (fetchTarball https://github.com/justinwoo/easy-purescript-nix/archive/d0f592b71b2be222f8dcfb4f4cefb52608bbc1ae.tar.gz) { inherit pkgs; };
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
