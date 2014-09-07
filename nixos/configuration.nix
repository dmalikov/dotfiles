# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  nixpkgs.config.allowUnfree = true;

  networking.firewall.enable = false;
  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "ter-116n";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = [
      pkgs.terminus_font
    ];
  };

  environment = with pkgs; {
    systemPackages = [
      curl
      dhcp
      git
      haskellPackages.hdevtools
      htop
      iotop
      nix-repl
      nmap
      tig
      tmux
      tree
      vim
      wget
      wgetpaste
      zsh
    ];
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.transmission = {
    enable = true;
  };

  users = {
    defaultUserShell = "/var/run/current-system/sw/bin/zsh";
    extraUsers.yep = {
      name = "yep";
      group = "users";
      uid = 1000;
      createHome = true;
      home = "/home/yep";
      extraGroups = [ "wheel" ];
      useDefaultShell = true;
    };
  };
}
