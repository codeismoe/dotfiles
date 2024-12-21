{ config, pkgs, ... }:
let
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-full fontawesome roboto;
  });
in
{
  programs.home-manager.enable = true;
  home.stateVersion = "23.11";
  home.username = "lily";
  home.homeDirectory = "/home/lily";

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
    iosevka
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
    spotify
    stack
    tex
    unzip
    vscode
    wget
    openjdk
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
    userName = "Lily Steidel";
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
