{ pkgs ? import <nixpkgs> {}
}:
let
  type-level-json = pkgs.haskell.packages.ghc924.callCabal2nix "type-level-json" ./. {};
in
{
  inherit pkgs;
  inherit type-level-json;
}
