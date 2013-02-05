X=500
Y=0
TA="right"
HEIGHT=10
FG="#dddddd"
BG="#151515"
FONT="Montecarlo-10"

BATTERY_CRITICAL=10
SEP=" "

COLOR_ICON="#707070"
COLOR_BATTERY="#ffff66"
COLOR_BATTERY_CRITICAL="#ff4444"
COLOR_CLOCK="#66ff66"
COLOR_NETWORK="#ffff66"
COLOR_NOW_PLAYING="#8888ff"
COLOR_VOLUME="#ffff66"

ICON_BATTERY_CRITICAL="/home/dan/icons/sm4tik/bat_low_01.xbm"
ICON_BATTERY="/home/dan/icons/sm4tik/bat_full_01.xbm"
ICON_CLOCK="/home/dan/icons/sm4tik/clock.xbm"
ICON_NETWORK="/home/dan/icons/sm4tik/wifi_02.xbm"
ICON_NOW_PLAYING="/home/dan/icons/sm4tik/note.xbm"
ICON_VOLUME="/home/dan/icons/sm4tik/spkr_01.xbm"

icon(){
  echo "^fg($COLOR_ICON)^i($1)^fg()"
}

now_playing(){
  current=$(ncmpcpp --now-playing)
  echo "^ca(1, ncmpcpp toggle)^ca(2, ncmpcpp prev)^ca(3, ncmpcpp next)^fg($COLOR_NOW_PLAYING)$current^fg()^ca()^ca()^ca()"
}

battery(){
  percent=$(acpi | cut -d "," -f 2 | tr -d " %")
  status=$(acpi | cut -d " " -f 3 | tr -d ",")
  if [ $percent -le $BATTERY_CRITICAL ] && [ $status == "Discharging" ]; then
    echo "$(icon $ICON_BATTERY_CRITICAL) ^fg($COLOR_BATTERY_CRITICAL)$percent%^fg()"
  else
    echo "$(icon $ICON_BATTERY) ^fg($COLOR_BATTERY)$percent%^fg()"
  fi
}

network(){
  percent=$(cat /proc/net/wireless | grep wlan0 | cut -d ' ' -f 6 | tr -d '.')
  echo $percent%
}

volume(){
  vol=$(amixer get Master | egrep -o "[0-9]+%")
  echo "^ca(1, amixer -q set Master 3%-)^ca(3, amixer -q set Master 3%+)^ca(2, amixer -q set Master toggle)$vol^ca()^ca()^ca()"
}

clock(){
  echo $(date +%H:%M)
}

while :; do
  echo -n "$(icon $ICON_NOW_PLAYING) $(now_playing)$SEP"
  echo -n "$(battery)$SEP"
  echo -n "$(icon $ICON_NETWORK) ^fg($COLOR_NETWORK)$(network)^fg()$SEP"
  echo -n "$(icon $ICON_VOLUME) ^fg($COLOR_VOLUME)$(volume)^fg()$SEP"
  echo "$(icon $ICON_CLOCK) ^fg($COLOR_CLOCK)$(clock)^fg() "
  sleep 3
done | dzen2 -x $X -y $Y -ta $TA -h $HEIGHT -fg $FG -bg $BG -fn $FONT
