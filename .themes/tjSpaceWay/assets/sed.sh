#!/bin/sh
sed -i \
         -e 's/#000000/rgb(0%,0%,0%)/g' \
         -e 's/#cdcdcd/rgb(100%,100%,100%)/g' \
    -e 's/#000000/rgb(50%,0%,0%)/g' \
     -e 's/#7d8fa3/rgb(0%,50%,0%)/g' \
     -e 's/#1c1c1c/rgb(50%,0%,50%)/g' \
     -e 's/#cdcdcd/rgb(0%,0%,50%)/g' \
	"$@"
