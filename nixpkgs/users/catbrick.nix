{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;
  home.stateVersion = "23.11";
  home.username = "catbrick";
  home.homeDirectory = "/home/catbrick";

  fonts.fontconfig.enable = true;
  home.packages =  with pkgs; [
    cinny-desktop
    ledger
    google-chrome
    any-nix-shell
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    automake
    blueman
    brightnessctl
    cabal-install
    clang
    clang-tools
    curl
    deluge
    discord
    docker-compose
    file
    firefox
    qutebrowser
    ghc
    ghostscript
    glib
    gnumake
    gnupg
    haskell-language-server
    htop
    ispell
    jq
    meson
    mpv
    neofetch
    nethack
    nodejs
    openjdk
    pavucontrol
    python3
    qemu
    ripgrep
    ripgrep
    roboto
    rustup
    silver-searcher
    texliveFull
    unzip
    vintagestory
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

  programs.offlineimap = {
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
        config = ./init.org;
        alwaysEnsure = true;
        defaultInitFile = true;
        alwaysTangle = true;
    };
  };
  services.lorri.enable = true;
  services.emacs.enable = true;
}
