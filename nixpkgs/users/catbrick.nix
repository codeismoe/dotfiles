{ config, pkgs, ... }:
{
  programs.home-manager.enable = true;
  home.stateVersion = "23.11";
  home.username = "catbrick";
  home.homeDirectory = "/home/catbrick";

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
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
    emacs
    emacsPackages.mu4e
    file
    firefox
    ghc
    ghostscript
    glib
    gnumake
    haskell-language-server
    htop
    ispell
    jq
    meson
    mpv
    mu
    neofetch
    nethack
    nodejs
    openjdk
    pavucontrol
    python3
    qemu
    ripgrep
    roboto
    rustup
    silver-searcher
    texliveFull
    unzip
    vintagestory
    wget
    ripgrep
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

  services.lorri.enable = true;
  services.emacs.enable = true;
}
