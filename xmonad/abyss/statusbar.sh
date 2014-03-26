X=976
Y=0
TA="right"
HEIGHT=16
WIDTH=744
FG="#dddddd"
BG="#151515"
FONT="Montecarlo-10"

BATTERY_CRITICAL=10
SEP=" "

COLOR_ICON="#707070"
COLOR_NOW_PLAYING="#bbbbbb"
COLOR_VOLUME="#bbbbbb"

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

while :; do
  #echo -n "$(icon $ICON_NOW_PLAYING) $(now_playing)$SEP"
  echo "$(icon $ICON_VOLUME) ^fg($COLOR_VOLUME)$(volume)^fg()$SEP"
  sleep 5
done | dzen2 -x $X -y $Y -ta $TA -h $HEIGHT -w $WIDTH -fg $FG -bg $BG -fn $FONT
