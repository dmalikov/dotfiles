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

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless.

  environment = with pkgs; {
    systemPackages = [
      curl
      git
      htop
      iotop
      nix-repl
      nmap
      tig
      tmux
      tree
      vifm
      vim
      wget
      zsh
    ];
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  programs.zsh.enable = true;

  users = {
    defaultUserShell = "/var/run/current-system/sw/bin/zsh";
    extraUsers.m = {
      name = "m";
      group = "users";
      extraGroups = [ "wheel" ];
      uid = 1000;
      createHome = true;
      home = "/home/m";
      useDefaultShell = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCjDXb8JmdWxW40RS7TMm8xz9kadlo+CAdYrgGps5cIAXBD6+fdLbEjB66vu9YxG/WUUN7lOdRvh/aYJqzh+yLGD06/JSTZXdn8NnCkYBV70bUNzp/GqYaX+t7UdL8eF60p8HOQQVmOYUjbPX0eBEi4b6iDoT+3mO2m5CbLl19iJadTjB0lbJyRNC6BECHhUC+bCXaCLRLTr66f8qShAXup7UAi9v92UXPfm0c+ZaVWoIEjQnRH15VJoFo6geHhmUTSK8pJq+pUCpEiP9ryV/7fBuw2o9OVrP95dxt48cvjOnXq4wGj+DKvG7eCXKM8jC3tJOaUAktgk4Ky/x/fXv+v"
      ];
    };
  };

  fileSystems."/mnt/src" = { 
    fsType = "vboxsf";
    device = "src";
    options = "rw";
  };

}
