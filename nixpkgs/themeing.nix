{ config, pkgs, inputs, ... }:
let

in
{
  home-manager.users.catbrick = {
    programs = {
      kitty = {
        font = { name = "Iosevka Mono"; size = 12; };
        extraConfig = builtins.readFile (config.scheme {
          templateRepo = inputs.tinted-terminal;
          target = "kitty-base16";
          use-ifd = "always";
        });
      };

      waybar = let
        waybar-file = builtins.readFile "${inputs.config-files.packages.x86_64-linux.default.waybar-config.outPath}/waybar.json";
        colors = builtins.readFile (config.scheme inputs.tinted-waybar);
        styles = builtins.readFile "${inputs.config-files.packages.x86_64-linux.default.waybar-config.outPath}/waybar.css";
      in {
        style = ''
          ${colors}
          ${styles}
        '';
        settings = [(builtins.fromJSON waybar-file)];
      };
    };
    
    wayland.windowManager.sway = let
      sway-config = builtins.readFile "${inputs.config-files.packages.x86_64-linux.default.sway-config.outPath}/sway-config";
    in {
      enable = true;
      extraConfigEarly = builtins.readFile (config.scheme {
        templateRepo = inputs.base16-sway;
        target = "colors";
      });
      extraConfig = sway-config;
      config = null;
    };
  };
}
