{ packageName ? "localbin", pkgs ? (import <nixpkgs> {}) }:

pkgs.stdenv.mkDerivation {
  description = "localbin scripts";
  name = packageName;
  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    install -t $out/bin *.sh
  '';
}
