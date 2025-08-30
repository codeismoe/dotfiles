{config, pkgs, lib, ...}:

{
  programs.niri.settings = {
    prefer-no-csd = true;
    spawn-at-startup = [
      { command = ["systemctl --user reset-failed waybar.service"]; }
      { command = ["swww img ${config.stylix.image}" ]; }
    ];

    layout = {
      gaps = 2;
      struts.left = 2;
      struts.right = 2;
      border.width = 2;
      always-center-single-column = true;
      empty-workspace-above-first = false;
      tab-indicator = {
        position = "top";
        gaps-between-tabs = 10;
      };
    };

    overview.zoom = 0.5;
    screenshot-path = "~/Pictures/Screenshots/%Y-%m-%dT%H:%M:%S.png";

    switch-events =
      with config.lib.niri.actions;
      let
        sh = spawn "sh" "-c";
      in {
        tablet-mode-on.action = sh "notify-send tablet-mode-on";
        tablet-mode-off.action = sh "notify-send tablet-mode-off";
        lid-open.action = sh "notify-send lid-open";
        lid-close.action = sh "notify-send lid-close";
      };
    binds =
      with config.lib.niri.actions;
      with lib;
      let
        sh = spawn "sh" "-c";
        binds =
          {
            suffixes,
            prefixes,
            substitutions ? { },
          }:
          let
            replacer = replaceStrings (attrNames substitutions) (attrValues substitutions);
            format =
              prefix: suffix:
              let
                actual-suffix =
                  if isList suffix.action then
                    {
                      action = head suffix.action;
                      args = tail suffix.action;
                    }
                  else
                    {
                      inherit (suffix) action;
                      args = [ ];
                    };

                action = replacer "${prefix.action}-${actual-suffix.action}";
              in
                {
                  name = "${prefix.key}+${suffix.key}";
                  value.action.${action} = actual-suffix.args;
                };
            pairs =
              attrs: fn:
              concatMap (
                key:
                fn {
                  inherit key;
                  action = attrs.${key};
                }
              ) (attrNames attrs);
          in
            listToAttrs (pairs prefixes (prefix: pairs suffixes (suffix: [ (format prefix suffix) ])));
      in attrsets.mergeAttrsList [
        {
          "Mod+E".action = sh "emacsclient -nc";
          "Mod+Return".action = spawn "kitty";
          "Mod+Z".action = show-hotkey-overlay;
          "Mod+Space".action = spawn "fuzzel";
          "Mod+O".action = toggle-overview;

          "Mod+Shift+Print".action = screenshot;
          "Print".action.screenshot-screen = [ ];
          "Mod+Print".action = screenshot-window;

          "XF86AudioRaiseVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
          "XF86AudioLowerVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
          "XF86AudioMute".action = sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";

          "XF86MonBrightnessUp".action = sh "brightnessctl set 10%+";
          "XF86MonBrightnessDown".action = sh "brightnessctl set 10%-";

          "Mod+Q".action = close-window;

          "Mod+T".action = toggle-column-tabbed-display;
          "Mod+Tab".action = focus-column-right;
          "Mod+Shift+Tab".action = focus-column-left;
        }
        (binds {
          suffixes."H" = "column-left";
          suffixes."J" = "window-down";
          suffixes."K" = "window-up";
          suffixes."L" = "column-right";
          prefixes."Mod" = "focus";
          prefixes."Mod+Shift" = "move";
        })
        {
          "Mod+V".action = switch-focus-between-floating-and-tiling;
          "Mod+Shift+V".action = toggle-window-floating;
        }
        (binds {
          suffixes."I" = "workspace-down";
          suffixes."U" = "workspace-up";
          prefixes."Mod" = "focus";
          prefixes."Mod+Ctrl" = "move-window-to";
          prefixes."Mod+Shift" = "move";
        })
        (binds {
          suffixes = builtins.listToAttrs (
            map (n: {
              name = toString n;
              value = ["workspace" n];
            }) (range 1 9)
          );
          prefixes."Mod" = "focus";
          prefixes."Mod+Shift" = "move-window-to";
        })
        {
          "Mod+Comma".action = consume-window-into-column;
          "Mod+Period".action = expel-window-from-column;

          "Mod+R".action = switch-preset-column-width;
          "Mod+F".action = maximize-column;
          "Mod+Shift+F".action = fullscreen-window;
          "Mod+C".action = center-column;

          "Mod+Shift+Escape".action = toggle-keyboard-shortcuts-inhibit;
          "Mod+Shift+Q".action = quit;
          "Mod+Shift+P".action = spawn "swaylock";
        }
      ];
    outputs."eDP-1".scale = 1.0;
    environment."NIXOS_OZONE_WL" = "1";
    xwayland-satellite = {
      enable = true;
      path = lib.getExe pkgs.xwayland-satellite-unstable;
    };
  };
}
