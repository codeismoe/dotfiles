{ pkgs ? import <nixpkgs> {} }:
let
  packages = with pkgs; [ gnumake stow ];
in pkgs.mkShell {
  inputsFrom = [];
  buildInputs = packages;
}
