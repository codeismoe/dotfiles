{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;
  home.stateVersion = "23.11";
  home.username = "catbrick";
  home.homeDirectory = "/home/catbrick";

  fonts.fontconfig.enable = true;
  home.packages =  with pkgs; [
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
  ];

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

  programs.starship = {
    enable = true;
  };

  programs.kitty = {
    enable = true;
  };

  programs.waybar = {
    enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
        package = pkgs.emacs-git-pgtk;
        config = ./catbrick-emacs.org;
        defaultInitFile = true;
        alwaysEnsure = true;
        alwaysTangle = true;
    };
    # extraPackages = epkgs: [epkgs.];
  };

  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    pass = {
      enable = true;
      package = pkgs.rofi-pass-wayland;
    };
  };

  services.lorri.enable = true;
  services.emacs.enable = true;
  services.mbsync = {
    enable = true;
    configFile = ./mbsync;
    postExec = "${pkgs.mu}/bin/mu index";
  };

  services.gpg-agent = {
    enable = true;
  };
}
