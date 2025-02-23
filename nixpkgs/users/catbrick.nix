{ config, pkgs, ... }:
{
  programs.home-manager.enable = true;
  home.stateVersion = "23.11";
  home.username = "catbrick";
  home.homeDirectory = "/home/catbrick";

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    any-nix-shell
    roboto
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
    ghostscript
    glib
    gnumake
    haskell-language-server
    htop
    ispell
    kitty
    maim
    meson
    mu
    nethack
    nodejs
    pavucontrol
    python3
    qemu
    ripgrep
    rustup
    silver-searcher
    unzip
    wget
    openjdk
    blueman
    zoom-us
    dotnetCorePackages.dotnet_8.sdk
    omnisharp-roslyn
    deluge
    texliveFull
    mpv
    maptool
    vintagestory
    jq
    file
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

  services.lorri.enable = true;
  services.emacs.enable = true;
}
