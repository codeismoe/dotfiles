# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, ... }:

let
  sway-conf = pkgs.writeText "sway-regreet-config" ''
    exec "${config.programs.regreet.package}/bin/regreet; ${config.programs.sway.package}/bin/swaymsg exit"
    include /etc/sway/config.d/*
  '';

  sway-launcher = pkgs.writeScript "sway-launcher.sh" ''
    #!${pkgs.bash}/bin/bash

    source /etc/profile

    export MOZ_ENABLE_WAYLAND="1"
    export QT_QPA_PLATFORM="wayland;xcb"
    export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
    export GTK_USE_PORTAL=1
    export NIXOS_XDG_OPEN_USE_PORTAL=1
    # Fix for some Java AWT applications (e.g. Android Studio),
    # use this if they aren't displayed properly:
    export _JAVA_AWT_WM_NONREPARENTING=1
    export XDG_CURRENT_DESKTOP=sway
    export XDG_SESSION_DESKTOP=sway
    export XDG_SESSION_TYPE=wayland
    export GIO_EXTRA_MODULES=${pkgs.gvfs}/lib/gio/modules

    exec ${pkgs.sway}/bin/sway --config ${sway-conf}
  '';
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # used for vintage story lol
  nixpkgs.config.permittedInsecurePackages = [
    "dotnet-runtime-7.0.20"
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;


  nix.settings.experimental-features = [ "nix-command flakes" ];

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
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

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  fonts.packages = with pkgs; [
    nerd-fonts.iosevka
    nerd-fonts.fira-code
    source-sans
    noto-fonts
    noto-fonts-emoji
    iosevka
    fira-code
    fira-code-symbols
  ];

  services.dbus.enable = true;
  services.desktopManager.plasma6.enable = true;

  xdg.icons.enable = true;
  xdg.mime.enable = true;
  xdg.portal = {
    enable = true;
    config = {
      sway = {
        "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
      };
    };
    wlr.enable = true;
    # gtk portal needed to make gtk apps happy
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups = {
    catbrick = {};
  };

  users.users.catbrick = {
    isNormalUser = true;
    group = "catbrick";
    description = "CatBrick";
    extraGroups = [ "networkmanager" "docker" "wheel" "audio" "video" "power" "games" "libvirtd" ];
    shell = pkgs.fish;
  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    vim
    wget
    grim # screenshot functionality
    slurp # screenshot functionality
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    mako # notification system developed by swaywm maintainer
    rofi
    waybar
    networkmanagerapplet
    wineWowPackages.stable
    winetricks
    wineWowPackages.waylandFull
  ];

  # Enable the gnome-keyring secrets vault.
  # Will be exposed through DBus to programs willing to store secrets.
  services.gnome.gnome-keyring.enable = true;

  programs = {
    nix-ld.enable = true;
    regreet.enable = true;
    fish.enable = true;
    dconf.enable = true;
    gnupg.agent = {
      enable = true;
    };
    sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };
    steam.enable = true;
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${sway-launcher} --config ${sway-conf}";
        user = "greeter";
      };
    };
  };

  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;
  programs.virt-manager.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
