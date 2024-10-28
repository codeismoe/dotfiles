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
    emacs
    firefox
    ghc
    gnumake
    graphviz
    haskell-language-server
    iosevka
    ispell
    kitty
    maim
    nethack
    nodejs
    python3
    qemu
    ripgrep
    rustup
    signal-desktop
    silver-searcher
    spotify
    unzip
    wget
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

  programs.starship = {
    enable = true;
  };
}
