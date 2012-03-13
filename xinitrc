xsetroot -cursor_name left_ptr
setxkbmap -option "" -layout "us,ru" -option grp:caps_toggle -option grp_led:scroll
trayer --edge bottom --align left --transparent true --widthtype pixel --width 120 --heighttype pixel --height 20 --tint 0x1c1c1c --alpha 0 --SetDockType true &
nm-applet &
xscreensaver -no-splash &
exec ck-launch-session dbus-launch xmonad
