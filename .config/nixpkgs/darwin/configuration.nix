{ config, pkgs, ... }:

{
  imports = [ <home-manager/nix-darwin> ];
  home-manager.useUserPackages = true;

  environment.systemPackages = [];

  homebrew = {
    enable = true;
    brewPrefix = "/opt/homebrew/bin";
    casks = [ "firefox" "slack" "spotify" "discord" "runelite" "zoom" "krita" "gimp" "blender"];
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

  environment.systemPath = [ /run/current-system/sw/bin ];
  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";
  programs.fish.enable = true;

  home-manager.users.pks = {pkgs, ...}: {
    home.sessionVariables = {
      EDITOR = "nvim";
    };

    home.packages = (with pkgs; [
      timewarrior
      taskwarrior
      ripgrep

      darwin.apple_sdk.frameworks.Cocoa

      any-nix-shell
      direnv
      
      rustup

      docker
      docker-compose
      nodejs
    ]);

    programs.tmux = {
      enable = true;
      escapeTime = 0;
    };

    programs.git = {
      enable = true;
      userName = "Peter Steidel";
      userEmail = "pks@codeis.moe";
      ignores = [ ".DS_Store" "*~" "*.swp" ".vim"];
    };

    programs.fish = {
      enable = true;
      promptInit = ''
        any-nix-shell fish --info-right | source
        starship init fish | source
      '';

      plugins = [];
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

     programs.neovim = {
       enable = true;
       withPython3 = true;
       plugins = with pkgs.vimPlugins; [ vim-packer ];
       extraConfig = ''
           lua require('start')
       '';
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
    };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
