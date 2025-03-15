# shell.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.HUnit
  ];
  nativeBuildInputs = [
    pkgs.haskellPackages.haskell-language-server
  ];

  shellHook = ''
    cabal update
  '';
}