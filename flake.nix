{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay/";
    home-manager.url = "github:nix-community/home-manager";
    niri.url = "github:sodiboo/niri-flake";
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { home-manager, nixpkgs, niri, stylix, ... } @ inputs: {
    nixosConfigurations = {
      catbrick = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          stylix.nixosModules.stylix
          ./system.nix
          niri.nixosModules.niri
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "backup-";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users = {
              catbrick = import ./users/catbrick.nix { inherit inputs; };
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
