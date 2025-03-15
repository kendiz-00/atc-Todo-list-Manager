# default.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.developPackage {
  name = "todo-manager";
  src = ./.;
  version = "0.1.0.0";
  isLibrary = false;
  isExecutable = true;
  executable = {
    mainModule = "Main.hs";
  };
  buildInputs = [ pkgs.ghc pkgs.cabal-install pkgs.haskellPackages.HUnit ];
}