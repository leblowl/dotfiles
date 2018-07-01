# disable screen sleep
xset s off
xset -dpms
xset s noblank

# disable laptop screen for external monitor
xrandr --output eDP-1 --off

# map capslock to shift/escape
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'

# ???
xmodmap -e "pointer = 1 2 3 5 4"

# disable BEEP
xset b off

# start pulseaudio daemon
pulseaudio --start

# set desktop background
feh --bg-center ~/Pictures/wallpaper/debian-grey.png &

# start emacs server
emacs --daemon

# start i3 WM
exec i3
