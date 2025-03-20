{pkgs, system, ...}:

{
  waybar-config =
    pkgs.stdenv.mkDerivation {
      name = "waybar-config";
      src = ./.;
      phases = [ "installPhase" ];
      installPhase = ''
        mkdir $out
        sed 's,//.*$,,' $src/waybar.jsonc > $out/waybar.json
        cp $src/waybar.css $out/waybar.css
      '';
    };
  sway-config =
    pkgs.stdenv.mkDerivation {
      name = "sway-config";
      src = ./.;
      phases = [ "installPhase" ];
      installPhase = ''
        mkdir $out
        cp $src/bg.jpg $out/bg.jpg
        sed "s,BACKGROUND,$out/bg.jpg," $src/sway-config > $out/sway-config
      '';
    };
  emacs-config = pkgs.stdenv.mkDerivation {
    name = "catbrick-emacs";
    src = ./catbrick-emacs.org;
  };
}
