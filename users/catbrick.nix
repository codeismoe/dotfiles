{ inputs }:
{ config, pkgs, lib, ... }:

{
  imports = [
    ./catbrick-niri.nix
  ];

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
      openmw
      
      xwayland-satellite-unstable
    ];
  };

  programs.home-manager.enable = true;

  programs.tmux = {
    enable = true;
    escapeTime = 0;
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Cat Brick";
    userEmail = "me@lily.bike";
    ignores = [ ".DS_Store" "*~" "*.swp" ".vim"];
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
        any-nix-shell fish --info-right | source
        starship init fish | source
      '';
    plugins = [];
  };
  programs.starship.enable = true;

  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs-unstable-pgtk;
      config = ./catbrick-emacs.org;
      defaultInitFile = true;
      alwaysEnsure = true;
      alwaysTangle = true;
    };
  };
  stylix.targets.emacs.enable = false;
  services.emacs.enable = true;

  programs.fuzzel.enable = true;
  programs.swaylock.enable = true;

  programs.waybar = let
    waybar-style = builtins.readFile ./waybar.css;
  in {
    enable = true;
    systemd.enable = true;
    style = waybar-style;
    settings = [
      {
        "bar_id" = "bar-0";
        "ipc" = true;

        "spacing" = 4;
        "modules-left" = ["niri/workspaces"];
        "modules-center" = ["niri/window"];
        "modules-right" = [
          "pulseaudio"
          "backlight"
          "battery"
          "power-profiles-daemon"
          "clock"
          "tray"
        ];

        "niri/workspaces" = {
          "disable-scroll" = true;
          "all-outputs" = true;
          "warp-on-scroll" = false;
          "format" = "{name}";
        };

        "niri/window" = {
          "format" = "{}";
        };

        "tray" = {
          "spacing" = 10;
        };

        "clock" = {
          "tooltip-format" = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
          "format-alt" = "{:%Y-%m-%d}";
        };

        "cpu" = {
          "format" = "{usage}% ";
          "tooltip" = false;
        };

        "memory" = {
          "format" = "{}% ";
        };

        "backlight" = {
          "format" = "{percent}% {icon}";
          "format-icons" = ["" "" "" "" "" "" "" "" ""];
        };

        "battery" = {
          "states" = {
            "good" = 95;
            "warning" = 30;
            "critical" = 15;
          };

          "format" = "{icon}";
          "format-charging" = "{capacity}% ";
          "format-plugged" = "{capacity}% ";
          "format-alt" = "{time} {icon}";
          "format-good" = "{icon}";
          "format-full" = "";
          "format-icons" = ["" "" "" "" ""];
        };

        "power-profiles-daemon" = {
          "format" = "{icon}";
          "tooltip-format" = "Power profile = {profile}\nDriver = {driver}";
          "tooltip" = true;
          "format-icons" = {
            "default" = "";
            "performance" = "";
            "balanced" = "";
            "power-saver" = "";
          };
        };

        "pulseaudio" = {
          "scroll-step" = 1;
          "format" = "{volume}% {icon} {format_source}";
          "format-muted" = "{format_source}";
          "format-source" = "{volume}% ";
          "format-source-muted" = "";
          "format-icons" = {
            "headphone" = "";
            "default" = ["" "" ""];
          };
          "on-click" = "pavucontrol";
        };
      }];
  };

  services.swww.enable = true;
  services.lorri.enable = true;
  services.mako.enable = true;
  services.mbsync = {
    enable = true;
    configFile = ./mbsync;
    postExec = "${pkgs.mu}/bin/mu index";
  };
  services.gpg-agent.enable = true;

  fonts.fontconfig.enable = true;
}
