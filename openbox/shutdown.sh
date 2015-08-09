if [ $? = 0 ]; then
    zenity --question --text="Close $HOST?" --default-cancel
    systemctl poweroff

else
    exit
fi