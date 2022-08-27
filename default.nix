{ pkgs ? import <nixpkgs> {}
}:
let
  type-level-json = pkgs.haskell.packages.ghc922.callCabal2nix "type-level-json" ./. {};
in
{
  inherit pkgs;
  inherit type-level-json;
}
