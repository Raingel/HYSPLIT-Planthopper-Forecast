#!/bin/ksh

gs -dNOPAUSE -dSAFER -sDEVICE=pnmraw -q -sOutputFile=- $1.ps -c quit | convert - GIF:$1.gif

