source ${HOME}/prexinit

# run trayer
trayer --edge bottom --align left --transparent true --widthtype pixel --width 100 --heighttype pixel --height 10 --tint 0x080808 --alpha 0 --SetDockType true &

# run NetworkManager applet
nm-applet &

# run kbdd daemon
/usr/bin/kbdd &

# run xscreensaver daemon
xscreensaver -no-splash &

# process urxvt settings
xrdb ~/.Xdefaults
xrdb -override ~/.Xresources.scratchpad
xrdb -override ~/.Xresources.shiva
xrdb -override ~/.Xresources.large

# run unclutter to hide cursor
unclutter &

# run davmail to transport exchange server
davmail.sh &

# run ssh-agent
eval `ssh-agent`
ssh-add ~/.ssh/id_dsa

# run xmonad
~/.xmonad/xmobar-top &
exec ck-launch-session dbus-launch xmonad
