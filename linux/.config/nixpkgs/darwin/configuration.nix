{ config, pkgs, ... }:

{
  imports = [ <home-manager/nix-darwin> ];
  home-manager.useUserPackages = true;

  environment.systemPackages = [];

  homebrew = {
    enable = true;
    brewPrefix = "/opt/homebrew/bin";
    brews = [ 
        "python@3.10"
    ];
    casks = [
        "firefox"
        "slack"
        "spotify"
        "discord"
        "runelite"
        "zoom"
        "krita"
        "gimp"
        "blender"
    ];
  };

  fonts = {
    enableFontDir = true;
    fonts = [ (pkgs.nerdfonts.override { fonts = [ "Inconsolata" ]; }) ];
  };

  nix = {
    allowedUsers = [ "pks" ];
    package = pkgs.nix;
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  services.lorri.enable = true;

  environment.systemPath = [ /run/current-system/sw/bin ];
  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";
  programs.fish.enable = true;

  home-manager.users.pks = {pkgs, ...}: {
    home.sessionVariables = {
      EDITOR = "nvim";
    };

    home.packages = (with pkgs; [ any-nix-shell direnv ]);

    programs.tmux = {
      enable = true;
      escapeTime = 0;
    };

    programs.direnv = {
      enable = true;
    };

    programs.git = {
      enable = true;
      userName = "Peter Steidel";
      userEmail = "pks@codeis.moe";
      ignores = [ ".DS_Store" "*~" "*.swp" ".vim"];
    };

    programs.fish = {
      enable = true;

      plugins = [];

      interactiveShellInit = ''
        fish_add_path /opt/homebrew/opt/python@3.10/bin
        fish_add_path /usr/local/share/dotnet/x64
        fish_add_path /opt/homebrew/opt/openjdk/bin
        fish_add_path /Users/pks/.ghcup/bin/
        starship init fish | source
        source /opt/homebrew/share/fish/vendor_completions.d/*
      '';
    };

    programs.starship = {
      enable = true;
      settings = {
        nix_shell.symbol = " ";
        aws = { disabled=true; symbol = " "; };
        git_branch.symbol = " ";
        docker_context.symbol = " ";
        python.symbol = " ";
        elixir.symbol = " ";
        package.symbol = " ";
        rust.symbol = " ";
      };
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
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
