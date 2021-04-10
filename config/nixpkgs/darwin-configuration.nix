{ config, pkgs, ... }:

{
  imports = [ <home-manager/nix-darwin> ];
  home-manager.useUserPackages = true;

  environment.systemPackages = [ pkgs.neovim ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  environment.shells = [ pkgs.fish ];
  users.users.pks.shell = pkgs.bashInteractive;
  programs.fish.enable = true;

  home-manager.users.pks = { pkgs, ...}: {
    home.packages = with pkgs; [
      direnv
      elixir
      nodejs
      python3
      ghc
      cabal-install
      fish
      jq
      taskwarrior
      tasksh
      timewarrior
      git
      gnupg
      ripgrep
      neovim
      any-nix-shell
    ];

    programs.git = {
      enable = true;
      userName = "Peter Steidel";
      userEmail = "pks@codeis.moe";
    };

    programs.fish = {
      enable = true;
      interactiveShellInit = ''
        set -g theme_nerd_fonts yes
      '';
      promptInit = ''
        any-nix-shell fish --info-right | source
      '';

      plugins = [
        {
          name = "theme-bobthefish";
          src = pkgs.fetchFromGitHub {
            owner = "oh-my-fish";
            repo = "theme-bobthefish";
            rev = "626bd39b002535d69e56adba5b58a1060cfb6d7b";
            sha256 = "cd49e0a846601cb9a5cafa2254edccc094d216c603eeddd788feb233398c901b";
          };
        }
      ];
    };
  };

  system.defaults = {
    dock = {
      autohide = true;
      orientation = "bottom";
    };
    finder = {
      AppleShowAllExtensions = true;
      _FXShowPosixPathInTitle = true;
    };
    NSGlobalDomain = {
      _HIHideMenuBar = true;
      "com.apple.swipescrolldirection" = false;
    };
  };


  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
