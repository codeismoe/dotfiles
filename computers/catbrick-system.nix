# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, inputs, ... }:

{
  imports = [
    ./catbrick-hardware.nix
  ];

  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
    inputs.niri.overlays.niri
  ]; 
  
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nix = {
    settings.experimental-features = [ "nix-command flakes" ];
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = true;
    nvidiaSettings = true;
  };
  hardware.nvidia.prime = {
    intelBusId = "PCI:0:2:0";
    nvidiaBusId = "PCI:1:0:0";
    sync.enable = true;
  };
  hardware.bluetooth.enable = true;

  networking.hostName = "catbrick";
  networking.networkmanager.enable = true;


  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
  };
  services.avahi = {
    publish.enable = true;
    publish.userServices = true;
    enable = true;
    openFirewall = true;
  };
  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  services.blueman.enable = true;
  
  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  fonts.fontDir.enable = true;
  fonts.packages = (with pkgs; [
    nerd-fonts.iosevka
    nerd-fonts.fira-code
    source-sans
    noto-fonts
    noto-fonts-color-emoji
    iosevka
    fira-code
    fira-code-symbols
    _3270font
    ibm-plex
  ]);

  services.dbus.enable = true;
  services.power-profiles-daemon.enable = true;
  services.displayManager.gdm.enable = true;
  services.udisks2.enable = true;
  programs.dconf.enable = true;

  xdg.icons.enable = true;
  xdg.mime.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    # extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  users.groups = {
    catbrick = {};
  };

  users.users = {
    catbrick = {
      isNormalUser = true;
      group = "catbrick";
      description = "CatBrick";
      extraGroups = [
        "networkmanager"
        "docker"
        "wheel"
        "input"
        "audio"
        "video"
        "power"
        "games"
        "libvirtd"
      ];
      shell = pkgs.fish;
    };
  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    vim
    wget
    networkmanagerapplet
    git
    wineWowPackages.waylandFull
    cachix
  ];

  # Enable the gnome-keyring secrets vault.
  # Will be exposed through DBus to programs willing to store secrets.
  services.gnome.gnome-keyring.enable = true;
  services.openssh.enable = true;


  programs.nix-ld.enable = true;
  programs.fish.enable = true;
  programs.gnupg.agent.enable = true;
  programs.steam.enable = true;
  programs.hamster.enable = true;
  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable;
  };

  programs.virt-manager.enable = true;
  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;

  services.samba = {
    package = pkgs.samba4Full;
    enable = true;
    openFirewall = true;
    settings = {
      global = {
        "workgroup" = "WORKGROUP";
        "server string" = "SMBNIXSRV";
        "netbios name" = "SMBNIXNB";
        "security" = "user";
        "hosts allow" = "192.168.1. 127.0.0.1 localhost";
        "hosts deny" = "0.0.0.0/0";
        "guest account" = "nobody";
        "map to guest" = "Bad User";
      };
      "public" = {
        "path" = "/mnt/Shares/Public/";
        "browseable" = "yes";
        "read only" = "yes";
        "guest ok" = "yes";
      };
    };
  };

  services.samba-wsdd = {
    enable = true;
    openFirewall = true;
  };

  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/everforest.yaml";
    image = ../bg-3.jpg;
    polarity = "dark";
    
    fonts.monospace.package = pkgs.nerd-fonts.iosevka;
    fonts.monospace.name = "Iosevka Nerd Font";
    fonts.sansSerif.package = pkgs.nerd-fonts.ubuntu;
    fonts.sansSerif.name = "Ubuntu Nerd Font";
    fonts.serif = config.stylix.fonts.sansSerif;
    fonts.sizes.applications = 10;
    fonts.sizes.desktop = 12;

    cursor.package = pkgs.phinger-cursors;
    cursor.name = "phinger-cursors-dark";
    cursor.size = 12;
  };

  services.emacs.defaultEditor = true;
  
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
