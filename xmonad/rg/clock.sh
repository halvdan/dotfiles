X=810
Y=0
TA="center"
HEIGHT=16
WIDTH=166
FG="#dddddd"
BG="#151515"

COLOR_CLOCK="#dddddd"

clock(){
  echo $(date +%H:%M)
}

while :; do
  echo "^fg($COLOR_CLOCK)$(clock)^fg()"
  sleep 3
done | dzen2 -x $X -y $Y -ta $TA -h $HEIGHT -w $WIDTH -fg $FG -bg $BG
