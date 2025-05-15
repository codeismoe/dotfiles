{pkgs, ...}:

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
}
