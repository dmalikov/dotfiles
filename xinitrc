xsetroot -cursor_name left_ptr -solid "#080808"

# enable ctrl-alt-backspace sequence
setxkbmap -option terminate:ctrl_alt_bksp

# enable keyboard layout
setxkbmap -option "" -layout "us,ru" -option grp:caps_toggle

# enable right alt for xcompose
setxkbmap -option compose:ralt

# fix xcompose in gtk applications
export GTK_IM_MODULE=xim

# run trayer
trayer --edge bottom --align left --transparent true --widthtype pixel --width 100 --heighttype pixel --height 20 --tint 0x080808 --alpha 0 --SetDockType true &

# run NetworkManager applet
nm-applet &

# run kbdd daemon
/usr/bin/kbdd &

# run xscreensaver daemon
xscreensaver -no-splash &

# run xmonad
exec ck-launch-session dbus-launch xmonad
