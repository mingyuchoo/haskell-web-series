# default.nix
let
  pkgs = import <nixpkgs> {};
  compilerVersion = "ghc921";
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
  compiler.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (
      with pkgs.haskellPackages;
      [ cabal-install
        ghcid
      ]
    );
}
