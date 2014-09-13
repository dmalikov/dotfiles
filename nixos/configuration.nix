{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  nixpkgs.config.allowUnfree = true;

  networking.firewall.enable = false;
  networking.hostName = "nixos";
  networking.wireless.enable = false;

  i18n = {
    consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-i16n.psf.gz";
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

  programs.zsh.enable = true;

  environment = with pkgs; {
    systemPackages = [
      curl
      dhcp
      git
      haskellPackages.hdevtools
      haskellPackages.stylishHaskell
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
