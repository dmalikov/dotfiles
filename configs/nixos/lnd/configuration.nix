let
  pkgs = import <nixpkgs> {};
in

{ config
, pkgs
, ... }:

{
  imports =
    [ ./hardware-configuration.nix
      ./nginx.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.kernelParams = [ "console=ttyS0" ];
  boot.loader.grub.extraConfig = "serial; terminal_input serial; terminal_output serial";
  boot.loader.grub.device = "/dev/sda";

  environment = with pkgs; {
    systemPackages = [
      git gnumake gnupg htop iotop lsof nix-repl python stack tig tmux tree vim_configurable wget zsh
    ];
  };

  nix = {
    nixPath = [
      "nixpkgs=https://nixos.org/channels/nixos-17.09/nixexprs.tar.xz"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Amsterdam";

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    ports = [22229];
    passwordAuthentication = false;
  };

  services.logind.extraConfig = "RuntimeDirectorySize=1G";

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  boot.initrd.checkJournalingFS = false;

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
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC8uXQ9JJiB7fo6Ie8PReNveOf3xbUcUaq80YSbB2xD5tNAbPs6JWQ7ugOtd+trfmBIyfKqxrLhMF9/KdyqovVr7S1XvFD/gjyH5oo3X3qC7BLJJCPT+0jmC8BFrzUen3FrZno4VAoqzWF+xctl8MpYszywHBLpYxjKaOZW+OmrxFg5kKtF0djZPxd9gqUVuyDrT02gTHiQjRSusipz6OEF3aHjp6NJp1iOA95yzq79sHyZw//zqEiFDJhC3bkB1f31lKFdd1fu6NozBa54Hv9OepH/Ps1W33aRrdjdPDeJ5x+gQcZCzLF94mbh3JFRnQXeMRWm4o5sHcZuaUKMWR3t"
      ];
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";
}
