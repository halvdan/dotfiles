#!/bin/bash

(echo "first line" 
echo "second line" 
echo "third line" 
echo "fourth line" 
echo "fifth line") | dzen2 -p -x "500" -y "30" -w "220" -l "5" -sa 'l' -ta 'c' -title-name 'popup_sysinfo' -e 'onstart=uncollapse;button1=exit;button3=exit'                                                                    

