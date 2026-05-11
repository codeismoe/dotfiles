{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";

    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    niri.url = "github:sodiboo/niri-flake";
    niri.inputs.nixpkgs.follows = "nixpkgs";

    stylix.url = "github:nix-community/stylix/release-25.11";
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
          ./computers/catbrick-system.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.backupFileExtension = "backup-";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users = {
              catbrick = import ./users/user.nix;
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
