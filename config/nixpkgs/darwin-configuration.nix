{ config, pkgs, ... }:

let 
   pkgs_x86 = import <nixpkgs> { localSystem = "x86_64-darwin"; };
   pkgs_m1 = import <nixpkgs> { localSystem = "aarch64-darwin"; };
   vim-rescript = pkgs.vimUtils.buildVimPlugin {
     name = "vim-rescript";
     configurePhase = "";
     dontBuild = true;
     buildInputs = [];
     src = pkgs.fetchFromGitHub {
       owner = "rescript-lang";
       repo = "vim-rescript";
       rev = "a2196b50886f3009c846d11f361698620dfd51af";
       sha256 = "1x340ppcc6g9gfgx45qzr3wdiybxsgdavw1cq58jjw78vb2vfhib";
     };
   };
in
{
  imports = [ <home-manager/nix-darwin> ];
  home-manager.useUserPackages = true;

  environment.systemPackages = [];

  fonts = {
    enableFontDir = true;
    fonts = [ pkgs.nerdfonts ];
  };

  nix = {
    allowedUsers = [ "pks" ];
    package = pkgs.nix;
    extraOptions = ''
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  homebrew.enable = true;

  environment.systemPath = [ /run/current-system/sw/bin ];
  programs.fish.enable = true;

  home-manager.users.pks = {pkgs, ...}: {
    nixpkgs.overlays = [
      (self: super: { inherit (pkgs_x86) reattach-to-user-namespace;  })
    ];
    home.sessionVariables = {
      EDITOR = "nvim";
    };

    home.packages = (with pkgs; [
      timewarrior
      taskwarrior
      vit
      darwin.apple_sdk.frameworks.Cocoa
      direnv
      python39Packages.pip
      python39Packages.virtualenv
      python39Packages.pylint
      python39Packages.pynvim
      jq
      gnupg
      ripgrep
      rustup
      any-nix-shell
      docker
      docker-compose
      ccls

      nodejs
      yarn
      ocaml
      ocamlPackages.ocaml-lsp
    ]);

    programs.tmux = {
      enable = true;
      package = pkgs_x86.tmux;
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
       package = pkgs_x86.neovim-unwrapped;
       enable = true;
        withPython3 = true;
        plugins = with pkgs.vimPlugins; [
            vinegar
            vim-polyglot
            nord-vim
            tabular
            vim-markdown
            vim-commentary
            vim-airline
            vim-airline-themes
            vim-airline-clock

            { plugin = coc-nvim;
            config = ''
            '';
          }
            coc-snippets

            fzf-vim

            denite
            vim-snippets
            vim-rescript
        ];
        extraConfig = (builtins.readFile ./init.vim);
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
