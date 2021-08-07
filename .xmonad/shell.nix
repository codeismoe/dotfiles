{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = with pkgs; [
    (haskell.packages.ghc8104.ghcWithPackages (pkgs: with pkgs; [
      xmonad
      xmonad-utils
      xmonad-extras
      xmonad-contrib
      taffybar
    ]))
    haskell-language-server
    cabal-install
  ];
}
