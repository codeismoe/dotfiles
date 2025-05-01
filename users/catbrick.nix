{ inputs }:

{ config, pkgs, ... }:

{
  home = {
    stateVersion = "23.11";
    username = "catbrick";
    homeDirectory = "/home/catbrick";
    sessionVariables.NIXOS_OZONE_WL = "1";

    packages =  with pkgs; [
      any-nix-shell
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      automake
      blueman
      brightnessctl
      cabal-install
      cinny-desktop
      clang
      clang-tools
      curl
      deluge
      discord
      nwg-look
      qt6ct
      libsForQt5.qt5ct
      vesktop
      docker-compose
      file
      firefox
      ghc
      ghostscript
      glib
      gnumake
      gnupg
      google-chrome
      haskell-language-server
      htop
      torzu
      wdisplays
      dolphin-emu
      ispell
      jq
      ledger
      meson
      mpv
      mu
      neofetch
      nethack
      nodejs
      openjdk
      pass
      pavucontrol
      python3
      qemu
      qutebrowser
      ripgrep
      ripgrep
      roboto
      rustup
      silver-searcher
      texliveFull
      unzip
      vintagestory
      elixir
      elixir-ls
      tela-icon-theme
      catppuccin
      catppuccin-gtk
      catppuccin-qt5ct
      catppuccin-cursors
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
        waybar-file = builtins.readFile "${inputs.config-files.packages.x86_64-linux.default.waybar-config.outPath}/waybar.json";
        styles = builtins.readFile "${inputs.config-files.packages.x86_64-linux.default.waybar-config.outPath}/waybar.css";
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
        package = pkgs.emacs-git-pgtk;
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
