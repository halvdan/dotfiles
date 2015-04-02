X=811
Y=0
TA="right"
HEIGHT=16
WIDTH=1000
FG="#dddddd"
BG="#151515"
FONT="Terminus-10"

BATTERY_CRITICAL=10
SEP="   "

COLOR_ICON="#707070"
COLOR_NOW_PLAYING="#bbbbbb"
COLOR_VOLUME="#bbbbbb"
COLOR_CLOCK="#bbbbbb"
COLOR_LOCK="#555555"

ICON_NOW_PLAYING="/home/dan/dotfiles/icons/sm4tik/note.xbm"
ICON_VOLUME="/home/dan/dotfiles/icons/sm4tik/spkr_01.xbm"

icon(){
  echo "^fg($COLOR_ICON)^i($1)^fg()"
}

now_playing(){
  current=$(ncmpcpp --now-playing)
  echo "^ca(1, ncmpcpp toggle)^ca(2, ncmpcpp prev)^ca(3, ncmpcpp next)^fg($COLOR_NOW_PLAYING)$current^fg()^ca()^ca()^ca()"
}

volume(){
  vol=$(amixer get Master | egrep -o "[0-9]+%")
  echo "^ca(1, amixer -q set Master 3%-)^ca(3, amixer -q set Master 3%+)^ca(2, amixer -q set Master toggle)$vol^ca()^ca()^ca()"
}

clock(){
  echo $(date +%H:%M)
}

lock(){
  echo "^ca(1, xscreensaver-command -lock)lock^ca()"
}

keyboard(){
  #current=$(xkb-switch)
  current="swap"
  if [ $current == "us" ]
  then
    echo "^ca(1, setxkbmap -layout se)$current^ca()"
  else
    echo "^ca(1, setxkbmap -layout us)$current^ca()"
  fi
}


while :; do
  #echo -n "$(icon $ICON_NOW_PLAYING) $(now_playing)$SEP"
  #echo -n "^fg($COLOR_VOLUME)$(keyboard)^fg()$SEP"
  echo -n "^fg($COLOR_LOCK)$(lock)^fg()$SEP"
  echo -n "$(icon $ICON_VOLUME) ^fg($COLOR_VOLUME)$(volume)^fg()$SEP"
  echo "^fg($COLOR_CLOCK)$(clock)^fg()$SEP"
  sleep 2
done | dzen2 -x $X -y $Y -ta $TA -h $HEIGHT -w $WIDTH -fg $FG -bg $BG
