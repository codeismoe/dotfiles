{
  description = "NixOS configuration";

  nixConfig = {
    substituters = [ "https://cache.nixos-cuda.org" ];
    trusted-public-keys = [ "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M=" ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay/";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    niri.url = "github:sodiboo/niri-flake";
    niri.inputs.nixpkgs.follows = "nixpkgs";

    stylix.url = "github:danth/stylix";
    stylix.inputs.nixpkgs.follows = "nixpkgs";

    # nix-flatpak.url = "github:gmodena/nix-flatpak/?ref=latest";
  };

  outputs = { home-manager, stylix, niri, nixpkgs, ... } @ inputs: {
    nixosConfigurations = {
      catbrick = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          stylix.nixosModules.stylix
          niri.nixosModules.niri
          # nix-flatpak.nixosModules.nix-flatpak
          ./computers/catbrick-system.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "backup-";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users = {
              catbrick = import ./users/user.nix { inherit inputs; };
            };
          }
        ];
        specialArgs = {
          inherit inputs;
        };
      };
    };
  };
}
