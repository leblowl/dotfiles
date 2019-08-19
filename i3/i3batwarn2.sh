#!/bin/bash -x

#############################################
# This is a simple battery warning script.  #
# It uses i3's nagbar to display warnings.  #
#                                           #
#############################################

export DISPLAY=:0
export XAUTHORITY=/home/lucas/.Xauthority

/usr/bin/i3-msg fullscreen disable
/usr/bin/notify-send -u critical '<span>**DANGER**</span><br><span>Battery level low! Please charge this device.</span>'
