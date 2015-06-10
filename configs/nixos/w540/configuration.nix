{ config, pkgs, ... }:

let
  basic = with pkgs; [ curl dhcp fuse_exfat git gcc49 gnumake htop imagemagick iotop lsof nix-repl nmap python tig tmux transmission tree unzip vifm vim wget wgetpaste zsh ];
  x = with pkgs; [ dmenu i3status rxvt_unicode unclutter ];
in

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

  environment.systemPackages = basic ++ x;

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = [
      pkgs.terminus_font
    ];
  };

  i18n = {
    consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-i16n.psf.gz";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.xserver = {
    enable = false;
    synaptics.enable = true;
    windowManager = {
      default = "i3";
      i3.enable = false;
    };
    desktopManager = {
      default = "none";
      xterm.enable = false;
    };
    displayManager = {
      sessionCommands = ''
# enable ctrl-alt-backspace sequence
setxkbmap -option terminate:ctrl_alt_bksp

# enable keyboard layout
setxkbmap -option "" -layout "us,ru" -option grp:caps_toggle

# urxvt fonts
xset +fp /usr/share/fonts/terminus

# set up delay and rate
xset r rate 300 50

# enable right alt for xcompose
setxkbmap -option compose:ralt

# disable Display Power Managing Signaling
xset -dpms

# process urxvt settings
xrdb ~/.Xresources
xrdb -merge ~/.urxvt/colors/hybrid

# run unclutter to hide cursor
killall unclutter
unclutter &

exec i3 -V >> ~/.i3/logs 2>&1
      '';
    };
  };

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
      ];
    };
  };
}
