{ inputs, waybar-config }:

{ config, pkgs, ... }:

{
  home = {
    stateVersion = "23.11";
    username = "catbrick";
    homeDirectory = "/home/catbrick";
    sessionVariables.NIXOS_OZONE_WL = "1";

    packages =  with pkgs; [
      any-nix-shell

      # dev toops
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
      python3
      rustup
      
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

      # support
      blueman
      brightnessctl
      ghostscript
      gnupg
      
      # gaymes
      nethack
    ];
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
      font = {
        name = "Iosevka Mono"; size = 12;
      };
      themeFile = "Catppuccin-Frappe";
      shellIntegration.enableFishIntegration = true;
    };

    waybar =
      let
        waybar-file = builtins.readFile "${waybar-config.outPath}/waybar.json";
        styles = builtins.readFile "${waybar-config.outPath}/waybar.css";
      in {
        enable = true;
        style = ''
          ${styles}
        '';
        settings = [(builtins.fromJSON waybar-file)];
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

    rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
      pass = {
        enable = true;
        package = pkgs.rofi-pass-wayland;
      };
      theme = ./rofi.rasi;
    };
  };

  wayland.windowManager.hyprland = import ./catbrick-hyperland.nix;

  services = {
    hyprpaper = {
      enable = true;
      settings = {
        ipc = "on";
        splash = false;
        preload = ["~/Pictures/bg-3.jpg"];
        wallpaper = [
          ",~/Pictures/bg-3.jpg"
        ];
      };
    };

    hyprpolkitagent.enable = true;

    udiskie = {
      enable = true;
      automount = true;
      tray = "auto";
    };
    
    lorri.enable = true;
    emacs.enable = true;

    mbsync = {
      enable = true;
      configFile = ./mbsync;
      postExec = "${pkgs.mu}/bin/mu index";
    };
    gpg-agent = {
      enable = true;
    };
  };

  
  gtk = {
    enable = true;
    font = {
      name = "Iosevka Nerd Font";
    };
    theme = {
      name = "catpuccin-frappe";
      package = pkgs.catppuccin-gtk;
    };
    iconTheme = {
      name = "Tela";
      package = pkgs.tela-icon-theme;
    };
  };

  qt = {
    enable = true;
    style = {
      name = "catppuccin-frappe";
    };
  };

  fonts.fontconfig.enable = true;
}
