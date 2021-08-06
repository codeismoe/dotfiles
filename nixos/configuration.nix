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
  boot.kernelModules = [ "fuse" "kvm-amd" ];
  virtualisation.libvirtd.enable = true;
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "kvm"; # Define your hostname.

  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  # networking.ipv6 = false;
  # net
  networking.interfaces.enp7s0.useDHCP = true;

  nix = {
    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    binaryCaches = [
      "https://hydra.iohk.io"
    ];
  };
  
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  fonts.fonts = with pkgs; [
    # dejavu_fonts
    # iosevka
  ];

  environment.systemPackages = with pkgs; [
    vim 
    gtk-engine-murrine
    gtk_engines
    gsettings-desktop-schemas
    lxappearance
    trayer
  ];
  
  services.openssh.enable = true;
  programs.steam.enable = true;

  users.users.pks = {
    isNormalUser = true;
    shell = pkgs.fish;
    home = "/home/pks";
    description = "Peter Steidel";
    extraGroups = [ "wheel" "libvirtd" "docker" "power" "audio" ];
    openssh.authorizedKeys.keys = [ 
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCsw2JfSl7u08EFhz8qsljaEdAzhoGoXVz08bnuvXpmUUoRzyjcz99JAq6WJr+7OE+paiHUR1EifkduPIBwNscE92pvy1Ma1scTnCz2TP/qEJm4gutl0jHyrz0m4aLYWIQwYCmnxAc7WaCBhD4Af1leoTxoRP+08+U6D2+km6dVpNyTYNgpfyw9TEC0ooJXq89b2X0gpoS8L7WbGEMBmcLotLTnWIuDBR6x4NmbfP/Ag4paEBEB+s5T/ym4Ob63EnKF7GtJX64bnV5ghiwIPhua3Bmd9xaAjMsq7AU2ZN7TkEp8FLHrIDbd9IuzcQGlycOCgRl+g6jLQicC83ZMufhOqB1cRXzKQuUgKy6matQ/o++IDe+Xjh3dfnajy8Ke+ZrCyklkxDY3PSW1V9TQDk/tiEAtZgqXl6G9TjNmnucZZ7TZAJ2iFxhbOLGwbvmT/gYRLTCu9gEIp5SUYIOrkCwBjC3LyVXnBD0gI3td09Wyv2Y8ZCX+25FYMkF9ZqLHs1M= pks@Mac-mini"
    ];
  };

  services.dbus = {
    enable = true;
  };
    
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.vulkan_beta;
  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    displayManager.lightdm.enable = true;
    displayManager.defaultSession = "none+xmonad";
    displayManager.sessionCommands = ''
      xrandr --output HDMI-0 --primary --left-of HDMI-1
      trayer --edge top --align right --SetDockType true --SetPartialStrut true \
        --expand true --width 10 --transparent true --tint 0x5f5f5f --height 22 &
    '';
  };
  
  
  # Open ports in the firewall.
  networking.firewall.checkReversePath = false;
  networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}

