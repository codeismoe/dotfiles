# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  nixpkgs.config.allowUnfree = true;
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  hardware.pulseaudio.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.kernelModules = [ "fuse" ];
  virtualisation.libvirtd.enable = true;

  virtualisation.docker = {
    enable = true;
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.

  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  # networking.ipv6 = false;
  # net
  networking.interfaces.enp6s0.useDHCP = true;

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
    '';
    package = pkgs.nixUnstable;
  };
  
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  environment.systemPackages = with pkgs; [
    vim 
    virt-manager
    podman-compose
  ];
  
  services.openssh.enable = true;
  services.k3s = {
    enable = true;
    role = "server";
    extraFlags = toString [
      "--write-kubeconfig-mode 0644"
    ];
  };
  programs.steam.enable = true;
  programs.dconf.enable = true;

  users.users.pks = {
    isNormalUser = true;
    shell = pkgs.fish;
    home = "/home/pks";
    description = "Peter Steidel";
    extraGroups = [ "wheel" "libvirtd" "power" "audio" "docker" ];
  };

  security.sudo.wheelNeedsPassword=false;
  services.dbus = {
    enable = true;
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    desktopManager.plasma5.enable = true;
    displayManager.sddm.enable = true;
  };
  
  
  # Open ports in the firewall.
  networking.firewall.checkReversePath = false;
  networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}

