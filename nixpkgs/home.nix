{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;
  home.stateVersion = "23.11";
  home.username = "patchwork";
  home.homeDirectory = "/home/patchwork";

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Iosevka" "Inconsolata" ]; })
    any-nix-shell
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    automake
    brightnessctl
    cabal-install
    clang
    clang-tools
    curl
    discord
    docker-compose
    emacs
    emacsPackages.mu4e
    firefox
    ghc
    glib
    gnumake
    haskell-language-server
    htop
    iosevka
    ispell
    kitty
    maim
    meson
    mu
    nethack
    nodejs
    python3
    qemu
    ripgrep
    rustup
    silver-searcher
    spotify
    texliveFull
    unzip
    wget
    ghostscript
  ];

  programs.tmux = {
    enable = true;
    escapeTime = 0;
  };

  programs.direnv = {
    enable = true;
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

  services.lorri.enable = true;

  programs.starship = {
    enable = true;
  };
}
