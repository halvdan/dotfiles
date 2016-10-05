X=2080
Y=0
TA="right"
HEIGHT=16
WIDTH=920
FG="#dddddd"
BG="#151515"
FONT="Montecarlo-10"

SEP="   "

COLOR_ICON="#707070"
COLOR_NOW_PLAYING="#bbbbbb"

COLOR_MUSIC="#bbbbbb"
COLOR_KEYBOARD="#bbbbbb"
COLOR_VOLUME="#bbbbbb"
COLOR_CLOCK="#bbbbbb"

ICON_NEXT="/home/dan/dotfiles/icons/sm4tik/next.xbm"
ICON_PREVIOUS="/home/dan/dotfiles/icons/sm4tik/prev.xbm"
ICON_PLAY="/home/dan/dotfiles/icons/sm4tik/play.xbm"
ICON_PAUSE="/home/dan/dotfiles/icons/sm4tik/pause.xbm"

ICON_NOW_PLAYING="/home/dan/dotfiles/icons/sm4tik/note.xbm"
ICON_VOLUME="/home/dan/dotfiles/icons/sm4tik/spkr_01.xbm"

icon(){
    echo "^fg($COLOR_ICON)^i($1)^fg()"
}

music(){
    is_running=$(ps -ef | grep spotify | grep -v grep)
    if [[ $is_running ]]; then
        prev_track="dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
        play_pause="dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
        next_track="dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
        echo "^ca(1, $prev_track)^fg($COLOR_MUSIC)$(icon $ICON_PREVIOUS)^fg()^ca() ^ca(1, $play_pause)^fg($COLOR_MUSIC)$(icon $ICON_PLAY) $(icon $ICON_PAUSE)^fg()^ca() ^ca(1, $next_track)^fg($COLOR_MUSIC)$(icon $ICON_NEXT)^fg()^ca()"
    else 
        echo ""
    fi
}

volume(){
    vol=$(amixer get Master | egrep -o "[0-9]+%" | head -n 1)
    echo "^ca(1, amixer -q set Master 3%-)^ca(3, amixer -q set Master 3%+)^ca(2, amixer -q set Master toggle)$vol^ca()^ca()^ca()"
}

keyboard(){
    current=$(setxkbmap -query | grep layout | sed 's/ //g' | cut -d':' -f2)
    if [ $current == "us" ]
    then
        echo "^ca(1, setxkbmap -layout se)$current^ca()"
    else
        echo "^ca(1, setxkbmap -layout us)$current^ca()"
    fi
}

clock(){
    echo $(date +%H:%M)
}


while :; do
    echo -n "^fg($COLOR_MUSIC)$(music)^fg()$SEP"
    echo -n "^fg($COLOR_KEYBOARD)$(keyboard)^fg()$SEP"
    echo -n "$(icon $ICON_VOLUME) ^fg($COLOR_VOLUME)$(volume)^fg()$SEP"
    echo "^fg($COLOR_CLOCK) $(clock)^fg()$SEP"
    sleep 1
done | dzen2 -x $X -y $Y -ta $TA -h $HEIGHT -w $WIDTH -fg $FG -bg $BG -fn $FONT
