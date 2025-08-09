{ inputs }:
{ config, pkgs, lib, ... }:

let
  graphite-gtk = pkgs.callPackage ./graphite-gtk.nix { 
    themeVariants = [ "pink" "teal" "purple" ];
    sizeVariants = [ "standard" "compact" ];
    tweaks = [ "rimless" ];
    round = "0px";
  };

  waybar-config = pkgs.callPackage ../waybar-config/default.nix {};
in
{
  home = {
    stateVersion = "23.11";
    username = "catbrick";
    homeDirectory = "/home/catbrick";
    sessionVariables.NIXOS_OZONE_WL = "1";

    packages =  with pkgs; [
      any-nix-shell
      lutris
      hercules
      x3270

      # dev tools
      vscode
      automake
      cabal-install
      clang
      clang-tools
      docker-compose
      elixir
      elixir-ls
      ghc
      glib
      gnumake
      haskell-language-server
      kind
      kubectl
      meson
      nodejs
      openjdk
      python313Full
      virtualenv
      rustup
      ocaml
      opam
      ocamlformat
      ocamlPackages.merlin
      ocamlPackages.ocaml-lsp
      dune_3
      python313Packages.pip

      # cli
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      curl
      file
      htop
      ispell
      jq
      mu
      neofetch
      pass
      ripgrep
      texliveFull
      unzip

      # aesthetics
      catppuccin
      catppuccin-cursors
      catppuccin-gtk
      catppuccin-qt5ct
      roboto
      tela-icon-theme

      # applications
      deluge
      dolphin-emu
      firefox
      gimp
      google-chrome
      krita
      mpv
      pavucontrol
      qemu
      quickemu
      discord
      wdisplays
      nicotine-plus
      brightnessctl
      ghostscript
      gnupg
      
      # gaymes
      nethack

      xwayland-satellite-unstable
    ] ++ [graphite-gtk];
  };
  
  programs = {
    home-manager.enable = true;
    tmux = {
      enable = true;
      escapeTime = 0;
      
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    git = {
      enable = true;
      userName = "Cat Brick";
      userEmail = "me@lily.bike";
      ignores = [ ".DS_Store" "*~" "*.swp" ".vim"];
    };

    fish = {
      enable = true;
      interactiveShellInit = ''
        any-nix-shell fish --info-right | source
        starship init fish | source
      '';
      plugins = [];
    };

    starship = {
      enable = true;
    };

    kitty = {
      enable = true;
      shellIntegration.enableFishIntegration = true;
    };

    emacs = {
      enable = true;
      package = pkgs.emacsWithPackagesFromUsePackage {
        package = pkgs.emacs-unstable-pgtk;
        config = ./catbrick-emacs.org;
        defaultInitFile = true;
        alwaysEnsure = true;
        alwaysTangle = true;
      };
    };

    niri.settings = {
      prefer-no-csd = true;
      spawn-at-startup = [
        { command = ["systemctl --user reset-failed waybar.service"]; }
        { command = ["waybar"]; }
            
      ];
      layout = {
        gaps = 8;
        struts.left = 8;
        struts.right = 8;
        border.width = 4;
        always-center-single-column = true;
        empty-workspace-above-first = true;
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

            "Mod+Shift+S".action = screenshot;
            "Print".action.screenshot-screen = [ ];
            "Mod+Print".action = screenshot-window;

            "Mod+Insert".action = set-dynamic-cast-window;
            "Mod+Shift+Insert".action = set-dynamic-cast-monitor;
            "Mod+Delete".action = clear-dynamic-cast-target;

            "XF86AudioRaiseVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
            "XF86AudioLowerVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
            "XF86AudioMute".action = sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";

            "XF86MonBrightnessUp".action = sh "brightnessctl set 10%+";
            "XF86MonBrightnessDown".action = sh "brightnessctl set 10%-";

            "Mod+Q".action = close-window;

            "Mod+T".action = toggle-column-tabbed-display;

            "XF86AudioNext".action = focus-column-right;
            "XF86AudioPrev".action = focus-column-left;

            "Mod+Tab".action = focus-window-down-or-column-right;
            "Mod+Shift+Tab".action = focus-window-up-or-column-left;
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
            suffixes."Home" = "first";
            suffixes."End" = "last";
            prefixes."Mod" = "focus-column";
            prefixes."Mod+Ctrl" = "move-column-to";
          })
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
                value = [
                  "workspace"
                  (n + 1)
                ]; # workspace 1 is empty; workspace 2 is the logical first.
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

            "Mod+Minus".action = set-column-width "-10%";
            "Mod+Plus".action = set-column-width "+10%";
            "Mod+Shift+Minus".action = set-window-height "-10%";
            "Mod+Shift+Plus".action = set-window-height "+10%";

            "Mod+Shift+Escape".action = toggle-keyboard-shortcuts-inhibit;
            "Mod+Shift+Q".action = quit;
            "Mod+Shift+P".action = power-off-monitors;

          }
        ];
      outputs."eDP-1".scale = 1.0;
      environment."NIXOS_OZONE_WL" = "1";
      xwayland-satellite = {
        enable = true;
        path = lib.getExe pkgs.xwayland-satellite-unstable;
      };
    };

    fuzzel.enable = true;
    swaylock.enable = true;
    waybar = let
      waybar-settings = builtins.fromJSON (builtins.readFile "${waybar-config}/waybar.json");
      waybar-style = builtins.readFile "${waybar-config}/waybar.css";
    in {
        enable = true;
        systemd.enable = true;
        style = waybar-style;
        settings = [waybar-settings];
      };
  };

  services = {
    lorri.enable = true;
    emacs.enable = true;
    mako.enable = true;
    mbsync = {
      enable = true;
      configFile = ./mbsync;
      postExec = "${pkgs.mu}/bin/mu index";
    };
    gpg-agent = {
      enable = true;
    };
  };

  fonts.fontconfig.enable = true;

  stylix.targets = {
    emacs.enable = false;
  };
}
